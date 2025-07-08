with GNAT.CGI;
with http;
package body http is
   procedure listen(self: in out HTTP_Server; port: String) is
      CR: constant Character := ada.characters.latin_1.CR;
      LF: constant Character := ada.characters.latin_1.LF;
      resp: Stream_Element_Array := to_stream_element_array ("HTTP/1.1 200 OK" & CR & LF & CR & LF & "Request received!");
      res: Address_Info_Array := get_address_info(host => "", service => PORT, passive => true);
      data: Stream_Element_Array(1..1024);
      last: Stream_Element_Offset;
      req: HTTP_Request;
      
      --  reuse_option: Option_Type(Reuse_Address) := (name => Reuse_Address, enabled => true);
   
   begin
      if res'length < 1 then put_line(standard_error, "No available address to bind to"); end if;
      self.addr := res(1);
      -- make a socket, bind it, and listen on it
      put_line("Creating socket for " & image(self.addr));
      create_socket(self.socket, self.addr.addr.family, self.addr.mode, self.addr.level);
      --  set_socket_option(self.socket, self.addr.level, reuse_option);
      put_line("Binding socket to " & image(self.addr.addr));
      bind_socket(self.socket, self.addr.addr);
      put_line("Listening on " & image(self.addr.addr));
      listen_socket(self.socket);

      -- now accept an incoming connection
      while True loop begin
         accept_socket(self.socket, self.conn_socket, self.client_addr);
         put_line("Accepting connection from " & image(self.client_addr));
         -- ready to communicate on socket descriptor self.conn_socket
         receive_socket(self.conn_socket, data, last);
         req := parse_request (data);
         
         send_socket(self.conn_socket, resp, last);
         put_line(Long_Long_Integer'Image(Long_Long_Integer(last)) & " characters sent!");
         put_line("Closing connection to " & image(self.client_addr));
         close_socket(self.conn_socket);
         exception
         when Bad_Request => 
            put_line("400: Bad Request");
            -- TODO: Send 400 response
            -- TODO: close socket
            close_socket(self.conn_socket);
         end;
      end loop;
      
   end;

   function image(item: Address_Info) return String is
   begin
      return "family: " & Family_Type'image(item.addr.family) & " mode: " & Mode_Type'image(item.mode) & " level: " & Level_Type'image(item.level); 
   end image;

   function to_stream_element_array(str: String) return Stream_Element_Array is
      arr: Stream_Element_Array (1..str'length);
   begin
      for c in str'range loop
         arr(Stream_Element_Offset(c)) := Stream_Element(Character'pos(str(c)));
      end loop;
      return arr;
   end;

   function to_string(arr: Stream_Element_Array) return String is
      str: String(1..arr'length);
   begin
      for e in arr'range loop
         --  put_line("e: " & Stream_Element_Offset'Image(e));
         --  put_line("arr(e): " & Stream_Element'Image(arr(e)));
         str(Integer(e-arr'first+1)) := Character'val(arr(e));
      end loop;
      return str;
   end;

   function parse_request(req: Stream_Element_Array) return HTTP_Request is
      request: HTTP_Request;
      last: Stream_Element_Offset := 1;
   begin
      -- parse request line
      request.method := parse_request_method (req, last);
      parse_character (req, last, SPACE);
      request.uri := parse_request_uri(req, last);
      parse_character (req, last, SPACE);
      request.version := parse_http_version(req, last);
      parse_character (req, last, CR);
      parse_character (req, last, LF);
      -- if CRLF does not follow request line then there must be request headers
      --  if req(last) /= CR then
      --     request.headers := parse_request_headers (req, last);
      --  end if;
      --  -- parse empty line that is between headers and body/content
      --  parse_character (req, last, CR);
      --  parse_character (req, last, LF);
      -- parse body

      put_line(image(request));

      return request;

      exception
         when Unsafe_Character =>
            put_line("Unsafe Character");
            raise Bad_Request;
         when Bad_Method =>
            put_line("Bad Method");
            raise Bad_Request;
      
      --  return request;
   end;

   function parse_request_method(req: Stream_Element_Array; last: in out Stream_Element_Offset) return Request_Method is
      prev_last: Stream_Element_Offset := last;
   begin
      while last <= req'length and then req(last) /= SPACE loop
         if not is_in(Character'val(req(last)), TOKEN_CHAR) then
            raise Unsafe_Character;
         end if;
         last := last + 1;
      end loop;
      -- Method must be at least one TOKEN_CHAR
      if last - prev_last < 1 then
         raise Bad_Method;
      end if;
      return Request_Method'Value(to_string(req(1..last-1)));
         exception
            when Constraint_Error => 
               --  put_line(to_string(req(1..last-1)));
               raise Bad_Method;
   end;

   function parse_request_uri(req: Stream_Element_Array; last: in out Stream_Element_Offset) return Request_URI is
      start_uri: Stream_Element_Offset := last;
      segment_start: Stream_Element_Offset := last;
      param_name: Unbounded_String;
      uri: Request_URI;
      is_query: Boolean := false;
      is_param_value: Boolean := false;
   begin
      -- parse origin form (RFC 9112 3.2.1) = absolute-path [ "?" query ]
      -- query (RFC 3986 3.4) = *( pchar / "/" / "?" )
      -- absolute-path = 1*( "/" segment )
      -- segment = *pchar
      -- pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
      -- unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
      -- pct-encoded   = "%" HEXDIG HEXDIG
      -- sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
      --  Put_Line (Stream_Element'Image(req(last)) & " " & Character'Val(req(last)));
      if last <= req'length and then req(last) /= Character'Pos('/') then
         raise Bad_Request;
      end if;
      while last <= req'length and then req(last) /= SPACE loop
         --  Put_Line (Character'Val(req(last)) & "");
         if not is_query then -- parse path
            if req(last) = Character'Pos('/') then
               if segment_start /= start_uri then
                  uri.path.append(to_unbounded_string(to_string(req(segment_start..last-1))));
               end if;
               segment_start := last + 1;
            elsif req(last) = Character'Pos('?') then
               is_query := true;
               if last > segment_start then
                  uri.path.append(To_Unbounded_String(to_string(req(segment_start..last-1))));
               end if;
               segment_start := last + 1;
            elsif req(last) = Character'Pos('%') then
               if not (req'Length > last + 2 and then Is_In (Character'Val(req(last+1)), HEXDIG) and then Is_In (Character'Val(req(last+2)), HEXDIG)) then
                  raise Bad_Request;
               end if;
               last := last + 2;
            else
               if not Is_In (Character'Val(req(last)), PCHAR) then
                  raise Bad_Request;
               end if;
            end if;
         else -- TODO: parse query
            -- TODO: Refactor to account for the fact that param values are not required
            if req(last) = Character'Pos('=') then -- param name end
               param_name := To_Unbounded_String(To_String(req(segment_start..last-1)));
               segment_start := last + 1;
               is_param_value := true;
            elsif req(last) = Character'Pos('&') then -- param value end
               if not is_param_value then
                  Uri.query.Insert (To_Unbounded_String(To_String(req(segment_start..last-1))), To_Unbounded_String(""));
               else -- TODO: Check if value is empty after = (aka =&)
                  Uri.query.Insert (param_name, To_Unbounded_String(To_String(req(segment_start..last-1))));
               end if;
               segment_start := last + 1;
               is_param_value := false;
            end if;
         end if;
         last := last + 1;
      end loop;
      -- TODO: Capture remaining path segment or query pair
      if not is_query then
         if last > segment_start then
            uri.path.append(To_Unbounded_String(to_string(req(segment_start..last-1))));
         end if;
      else
         -- TODO: handle capturing last part of query
         null;
      end if;
      if last > req'length then
         raise Bad_Request;
      end if;
      return uri;
   end;

   function parse_http_version(req: Stream_Element_Array; last: in out Stream_Element_Offset) return HTTP_Version is
   begin
      -- check character requirement for HTTP/#.#
      if req'length < last + 7 then
         raise Bad_Request;
      end if;
      if not (req(last) = Character'pos('H') and
         req(last+1) = Character'pos('T') and
         req(last+2) = Character'pos('T') and
         req(last+3) = Character'pos('P') and
         req(last+4) = Character'pos('/') and
         req(last+5) = Character'pos('1') and
         req(last+6) = Character'pos('.')) then
            raise Bad_Request;
      end if;
      if req(last+7) = Character'pos('0') then last := last + 8; return HTTP_1_0;
      elsif req(last+7) = Character'pos('1') then last := last + 8; return HTTP_1_1;
      else raise Bad_Request; 
      end if;
   end;

   

   function parse_request_headers(req: Stream_Element_Array; last: in out Stream_Element_Offset) return String_Hashed_Maps.Map is
      headers: String_Hashed_Maps.Map;
   begin
      -- parse field name
      -- parse colon
      -- parse OWS
      -- parse field-value
      -- parse OWS
      -- parse CRLF
      return headers;
   end;

   procedure parse_character(req: Stream_Element_Array; last: in out Stream_Element_Offset; ch: Stream_Element) is
   begin
      if req(last) = ch then
         last := last + 1;
      else
         raise Bad_Request;
      end if;
   end;

   function is_valid_char(e: Stream_Element) return Boolean is
   begin
      return Is_In (Character'Val(e), CHAR);
   end;

   function image(req: HTTP_Request) return String is
      use Ada.Containers;
      LF : constant Character := Ada.Characters.Latin_1.LF;
      TAB : constant String := "  ";
      Root_URI : constant String := TAB & "uri: /," & LF;
      Is_Root_Path: Boolean := false;
      function uri_to_string return String is
         URI_Str: Unbounded_String;
      begin
         if req.uri.path.Length = 0 then
            return "/";
         else 
            for segment of req.uri.path loop
               Append (URI_Str, To_Unbounded_String("/"));
               Append (URI_Str, segment);
            end loop;
         end if;
         return to_string(URI_Str);
      end;
   begin
      return "{" & LF & 
               TAB & "method: " & Request_Method'image(req.method) & "," & LF &
               TAB & "uri: " & uri_to_string & "," & LF &
               TAB & "version: " & HTTP_Version'image(req.version) & "," & LF &
             "}";
   end;
end http;