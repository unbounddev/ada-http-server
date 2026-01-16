with http;

package body http is
   procedure listen (self : in out HTTP_Server; port : String) is
      CR        : constant Character := ada.characters.latin_1.CR;
      LF        : constant Character := ada.characters.latin_1.LF;
      resp      : Stream_Element_Array :=
        to_stream_element_array
          ("HTTP/1.1 200 OK" & CR & LF & CR & LF & "Request received!");
      res       : Address_Info_Array :=
        get_address_info (host => "", service => PORT, passive => true);
      data      : Stream_Element_Array (1 .. MAX_BUFFER);
      last      : Stream_Element_Offset := 1;
      lastParse : Stream_Element_Offset := 1;
      req       : HTTP_Request;
      status    : Parse_Status := REQ_METHOD;

      --  reuse_option: Option_Type(Reuse_Address) := (name => Reuse_Address, enabled => true);

   begin
      if res'length < 1 then
         put_line (standard_error, "No available address to bind to");
      end if;
      self.addr := res (1);
      -- make a socket, bind it, and listen on it
      put_line ("Creating socket for " & image (self.addr));
      create_socket
        (self.socket, self.addr.addr.family, self.addr.mode, self.addr.level);
      --  set_socket_option(self.socket, self.addr.level, reuse_option);
      put_line ("Binding socket to " & image (self.addr.addr));
      bind_socket (self.socket, self.addr.addr);
      put_line ("Listening on " & image (self.addr.addr));
      listen_socket (self.socket);

      -- now accept an incoming connection
      while True loop
         begin
            -- initialize parsing variables
            last := 1;
            lastParse := 1;
            status := REQ_METHOD;
            
            accept_socket (self.socket, self.conn_socket, self.client_addr);
            put_line ("Accepting connection from " & image (self.client_addr));
            -- ready to communicate on socket descriptor self.conn_socket
            loop
               begin
                  if last = 1 then
                     receive_socket
                       (self.conn_socket, data (1 .. MAX_BUFFER), last);
                  else
                     receive_socket
                       (self.conn_socket,
                        data (last - lastParse + 2 .. MAX_BUFFER),
                        last);
                  end if;
                  Put_Line (To_String (data));
                  Put_Line ("Last: " & Stream_Element_Offset'Image (last));
                  -- TODO: Keep track of where we left off processing headers
                  -- TODO: Keep requesting data until we have reached end of headers
                  -- TODO: Only request exact Content-Length (body) after headers received
                  -- TODO: Consider what the limit should be for header section (general consensus is base limit of 8KB)
                  parse_request (data, req, status, lastParse);
                  if status = REQ_COMPLETE then
                     exit;
                  end if;
                  data (1 .. last - lastParse + 1) := data (lastParse .. last);
                  Put_Line
                    ("Last Parse: " & Stream_Element_Offset'Image (lastParse));
                  if lastParse = 1 then
                     raise Bad_Request;
                  end if;
               exception
                  when Bad_Request =>
                     put_line ("Response: " & Parse_Status'Image (status));
                     put_line ("400: Bad Request");
                     -- TODO: Send 400 response
                     -- TODO: close socket
                     close_socket (self.conn_socket);
                  when Buffer_Overflow =>
                     if last < MAX_BUFFER then
                        raise Bad_Request;
                     end if;
                     Put_Line ("More data to receive...");
               end;
               exit when last < MAX_BUFFER;
            end loop;

            send_socket
              (self.conn_socket,
               to_stream_element_array
                 ("HTTP/1.1 200 OK" & CR & LF & CR & LF & image (req)),
               last);
            put_line
              (Long_Long_Integer'Image (Long_Long_Integer (last))
               & " characters sent!");
            put_line ("Closing connection to " & image (self.client_addr));
            close_socket (self.conn_socket);
         end;
      end loop;

   end;

   function image (item : Address_Info) return String is
   begin
      return
        "family: "
        & Family_Type'image (item.addr.family)
        & " mode: "
        & Mode_Type'image (item.mode)
        & " level: "
        & Level_Type'image (item.level);
   end image;

   function to_stream_element_array (str : String) return Stream_Element_Array
   is
      arr : Stream_Element_Array (1 .. str'length);
   begin
      for c in str'range loop
         arr (Stream_Element_Offset (c)) :=
           Stream_Element (Character'pos (str (c)));
      end loop;
      return arr;
   end;

   function to_string (arr : Stream_Element_Array) return String is
      str : String (1 .. arr'length);
   begin
      for e in arr'range loop
         --  put_line("e: " & Stream_Element_Offset'Image(e));
         --  put_line("arr(e): " & Stream_Element'Image(arr(e)));
         str (Integer (e - arr'first + 1)) := Character'val (arr (e));
      end loop;
      return str;
   end;

   procedure Parse_Request
     (data   : Stream_Element_Array;
      req    : in out HTTP_Request;
      status : in out Parse_Status;
      last   : in out Stream_Element_Offset)
   is
      lastParse : Stream_Element_Offset := 1;
   begin
      last := 1;
      -- TODO: Refactor parsing functions so that we know where we last safely left off and so we know how much of a buffer to keep
      while last <= data'Length loop
         case status is
            -- parse request line

            when REQ_METHOD =>
               begin
                  Put_Line ("Parsing Method...");
                  lastParse := last;
                  req.method := parse_request_method (data, last);
                  Put_Line (Request_Method'Image (req.method));
                  status := REQ_URI;
               exception
                  when Buffer_Overflow =>
                     last := lastParse;
                     return;
                  when Unsafe_Character =>
                     put_line ("Unsafe Character");
                     Put_Line (Parse_Status'Image (status));
                     raise Bad_Request;
                  when Bad_Method =>
                     put_line ("Bad Method");
                     Put_Line (Parse_Status'Image (status));
                     raise Bad_Request;
               end;

            when REQ_URI =>
               begin
                  Put_Line ("Parsing URI...");
                  lastParse := last;
                  Put_Line ("Parsing space...");
                  Put_Line (Character'Image (Character'Val (data (last))));
                  parse_character (data, last, SPACE);
                  req.uri := parse_request_uri (data, last);
                  status := REQ_VERSION;
               exception
                  when Buffer_Overflow =>
                     last := lastParse;
                     return;
               end;

            when REQ_VERSION =>
               begin
                  Put_Line ("Parsing VERSION...");
                  lastParse := last;
                  parse_character (data, last, SPACE);
                  req.version := parse_http_version (data, last);
                  status := REQ_HEADERS;
               exception
                  when Buffer_Overflow =>
                     last := lastParse;
                     return;
               end;

            when REQ_HEADERS =>
               Put_Line ("Parsing HEADER...");
               parse_character (data, last, CR);
               parse_character (data, last, LF);
               -- if CRLF does not follow request line then there must be request headers
               if data (last) /= CR then
                  req.headers := parse_request_headers (data, last);
               end if;
               
               -- parse empty line that is between headers and body/content
               parse_character (data, last, CR);
               parse_character (data, last, LF);
               -- server will not read body for GET request
               if req.method = GET then
                  status := REQ_COMPLETE;
               else
                  status := REQ_BODY;
               end if;
               

            when REQ_BODY =>
               Put_Line ("Parsing BODY...");
               
               -- parse body
               status := REQ_COMPLETE;

            when REQ_COMPLETE =>
               exit;
            when others =>
               null;
         end case;
      end loop;
      put_line (image (req));

   exception
      when Unsafe_Character =>
         put_line ("Unsafe Character");
         Put_Line (Parse_Status'Image (status));
         raise Bad_Request;
      when Bad_Method =>
         put_line ("Bad Method");
         Put_Line (Parse_Status'Image (status));
         raise Bad_Request;
      when Constraint_Error =>
         Put_Line (Parse_Status'Image (status));
         raise Bad_Request;
      when Buffer_Overflow =>
         last := lastParse;
         raise Buffer_Overflow;

         --  return request;
   end;

   function parse_request_method
     (req : Stream_Element_Array; last : in out Stream_Element_Offset)
      return Request_Method
   is
      prev_last : Stream_Element_Offset := last;
   begin
      while last <= req'length and then req (last) /= SPACE loop
         if not is_in (Character'val (req (last)), TOKEN_CHAR) then
            raise Unsafe_Character;
         end if;
         last := last + 1;
      end loop;
      -- Method must be at least one TOKEN_CHAR
      if last - prev_last < 1 then
         raise Bad_Method;
      end if;
      if last > req'Length then
         raise Buffer_Overflow;
      end if;
      return Request_Method'Value (to_string (req (prev_last .. last - 1)));
   exception
      when Constraint_Error =>
         raise Bad_Method;
   end;

   function parse_request_uri
     (req : Stream_Element_Array; last : in out Stream_Element_Offset)
      return Request_URI
   is
      start_uri      : Stream_Element_Offset := last;
      segment_start  : Stream_Element_Offset := last;
      param_name     : Unbounded_String;
      uri            : Request_URI;
      is_query       : Boolean := false;
      is_param_value : Boolean := false;
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
      Put_Line ("Parsing Root...");
      if last > req'Length then
         raise Buffer_Overflow;
      end if;
      Put_Line ("Parsing Root...");
      if req (last) /= Character'Pos ('/') then
         raise Bad_Request;
      end if;
      Put_Line ("Found Root...");
      while last <= req'length and then req (last) /= SPACE loop
         --  Put_Line (Character'Val(req(last)) & "");
         if not is_query then
            -- parse path
            if req (last) = Character'Pos ('/') then
               if segment_start /= start_uri then
                  -- TODO: Check for empty segment
                  uri.path.append
                    (to_unbounded_string
                       (to_string (req (segment_start .. last - 1))));
               end if;
               segment_start := last + 1;
            elsif req (last) = Character'Pos ('?') then
               is_query := true;
               if last > segment_start then
                  uri.path.append
                    (To_Unbounded_String
                       (to_string (req (segment_start .. last - 1))));
               end if;
               segment_start := last + 1;
            elsif req (last) = Character'Pos ('%') then
               if not (req'Length >= last + 2) then
                  raise Buffer_Overflow;
               end if;
               if not (Is_In (Character'Val (req (last + 1)), HEXDIG)
                       and then Is_In (Character'Val (req (last + 2)), HEXDIG))
               then
                  raise Bad_Request;
               end if;
               last := last + 2;
            else
               if not Is_In (Character'Val (req (last)), PCHAR) then
                  raise Unsafe_Character;
               end if;
            end if;
         else
            -- parse query
            if req (last) = Character'Pos ('=') then
               -- param name end
               param_name :=
                 To_Unbounded_String
                   (To_String (req (segment_start .. last - 1)));
               segment_start := last + 1;
               is_param_value := true;
            elsif req (last) = Character'Pos ('&') then
               -- param value end
               if not is_param_value then
                  Uri.query.Insert
                    (To_Unbounded_String
                       (To_String (req (segment_start .. last - 1))),
                     To_Unbounded_String (""));
               elsif segment_start = last - 1 then
                  -- Check if value is empty after = (aka =&)
                  Uri.query.Insert (param_name, To_Unbounded_String (""));
               else
                  Uri.query.Insert
                    (param_name,
                     To_Unbounded_String
                       (To_String (req (segment_start .. last - 1))));
               end if;
               segment_start := last + 1;
               is_param_value := false;
            end if;
            -- TODO: handle #... part
         end if;
         last := last + 1;
      end loop;
      if last > req'length then
         raise Buffer_Overflow;
      end if;
      -- Capture remaining path segment or query pair
      if not is_query then
         if last > segment_start then
            uri.path.append
              (To_Unbounded_String
                 (to_string (req (segment_start .. last - 1))));
         end if;
      else
         -- handle capturing last part of query
         if not is_param_value and last - 1 > segment_start then
            Uri.query.Insert
              (To_Unbounded_String
                 (To_String (req (segment_start .. last - 1))),
               To_Unbounded_String (""));
         elsif segment_start = last - 1 then
            Uri.query.Insert (param_name, To_Unbounded_String (""));
         else
            Uri.query.Insert
              (param_name,
               To_Unbounded_String
                 (To_String (req (segment_start .. last - 1))));
         end if;
         -- TODO: handle #... part
      end if;
      return uri;
   end;

   function parse_http_version
     (req : Stream_Element_Array; last : in out Stream_Element_Offset)
      return HTTP_Version is
   begin
      -- check character requirement for HTTP/#.#
      if req'length < last + 7 then
         raise Buffer_Overflow;
      end if;
      if not (req (last) = Character'pos ('H')
              and req (last + 1) = Character'pos ('T')
              and req (last + 2) = Character'pos ('T')
              and req (last + 3) = Character'pos ('P')
              and req (last + 4) = Character'pos ('/')
              and req (last + 5) = Character'pos ('1')
              and req (last + 6) = Character'pos ('.'))
      then
         raise Bad_Request;
      end if;
      if req (last + 7) = Character'pos ('0') then
         last := last + 8;
         return HTTP_1_0;
      elsif req (last + 7) = Character'pos ('1') then
         last := last + 8;
         return HTTP_1_1;
      else
         raise Bad_Request;
      end if;
   end;

   function Parse_Request_Headers
     (Req : Stream_Element_Array; Last : in out Stream_Element_Offset)
      return String_Hashed_Maps.Map
   is
      Headers     : String_Hashed_Maps.Map;
      Field_Name  : Unbounded_String;
      Field_Start : Stream_Element_Offset := Last;
      Value_Start : Stream_Element_Offset;
      Value_End   : Stream_Element_Offset;
   begin
      while Last <= Req'Length and then Req (Last) /= CR loop
         Field_Start := Last;
         -- parse field name
         while Last <= Req'length and then Req (Last) /= COLON loop
            if not Is_In (Character'val (Req (Last)), TOKEN_CHAR) then
               raise Bad_Request;
            end if;
            --  Put_Line (Character'Val (Req (Last)) & "");
            Last := Last + 1;
         end loop;
         Field_Name :=
           To_Unbounded_String (To_String (Req (Field_Start .. Last - 1)));
         --  Put_Line (To_String (Field_Name));
         -- parse colon
         Parse_Character (Req, Last, COLON);
         -- parse OWS
         while Last <= Req'length
           and then Is_In (Character'Val (Req (Last)), OWS)
         loop
            Last := Last + 1;
         end loop;
         Value_Start := Last;
         if Req (Last) /= CR then
            -- parse field-value
            while Last <= Req'length and then Req (Last) /= CR loop
               if not Is_In (Character'Val (Req (Last)), FIELD_VCHAR or OWS)
               then
                  raise Bad_Request;
               end if;
               if Is_In (Character'Val (Req (Last)), FIELD_VCHAR) then
                  Value_End := Last;
               end if;
               --  Put_Line (Character'Val (Req (Last)) & "");
               Last := Last + 1;
            end loop;
            Headers.Insert
              (Field_Name,
               To_Unbounded_String
                 (To_String (Req (Value_Start .. Value_End))));
            --  Put_Line
            --    (To_String (Field_Name)
            --     & ": "
            --     & To_String (Headers (Headers.Find (Field_Name))));
            --  Put_Line (Integer'Image (Req'Length));
            --  Put_Line (Stream_Element_Offset'Image (Last));
            -- parse CRLF
            Parse_Character (Req, Last, CR);
            Parse_Character (Req, Last, LF);
         else
            -- handle empty field value
            Headers.Insert (Field_Name, To_Unbounded_String (""));
            -- parse CRLF
            Parse_Character (Req, Last, CR);
            Parse_Character (Req, Last, LF);
         end if;
      end loop;
      return Headers;
   end;

   procedure Parse_Character
     (req  : Stream_Element_Array;
      last : in out Stream_Element_Offset;
      ch   : Stream_Element) is
   begin
      if req (last) = ch then
         last := last + 1;
      else
         raise Bad_Request;
      end if;
   end;

   function is_valid_char (e : Stream_Element) return Boolean is
   begin
      return Is_In (Character'Val (e), CHAR);
   end;

   function image (req : HTTP_Request) return String is
      use Ada.Containers;
      LF           : constant Character := Ada.Characters.Latin_1.LF;
      TAB          : constant String := "  ";
      Root_URI     : constant String := TAB & "uri: /," & LF;
      Is_Root_Path : Boolean := false;
      function uri_to_string return String is
         URI_Str  : Unbounded_String;
         Is_First : Boolean := true;
      begin
         Append
           (URI_Str, To_Unbounded_String ("{" & LF & TAB & TAB & "path: "));
         -- process path
         if req.uri.path.Length = 0 then
            Append (URI_Str, To_Unbounded_String ("[]"));
         else
            Append (URI_Str, To_Unbounded_String ("[ "));
            for segment of req.uri.path loop
               if Is_First then
                  Append (URI_Str, """" & segment & """");
                  Is_First := false;
               else
                  Append (URI_Str, ", """ & segment & """");
               end if;
            end loop;
            Append (URI_Str, To_Unbounded_String (" ]"));
         end if;
         -- process query
         Append
           (URI_Str, To_Unbounded_String ("," & LF & TAB & TAB & "query: "));
         if req.uri.query.Is_Empty then
            Append (URI_Str, To_Unbounded_String ("{}"));
         else
            Append (URI_Str, To_Unbounded_String ("{" & LF));
            for C in req.uri.query.Iterate loop
               Append (URI_Str, To_Unbounded_String (TAB & TAB & TAB & """"));
               Append (URI_Str, String_Hashed_Maps.Key (C));
               Append (URI_Str, To_Unbounded_String (""": """));
               Append (URI_Str, req.uri.query (C));
               Append (URI_Str, To_Unbounded_String ("""," & LF));
               -- TODO: If at the last element ommit the trailing comma
            end loop;
            Append (URI_Str, To_Unbounded_String (TAB & TAB & "}"));
         end if;
         Append (URI_Str, To_Unbounded_String (LF & TAB & "}"));
         return to_string (URI_Str);
      end;
   begin
      return
        "{"
        & LF
        & TAB
        & "method: "
        & Request_Method'image (req.method)
        & ","
        & LF
        & TAB
        & "uri: "
        & uri_to_string
        & ","
        & LF
        & TAB
        & "version: "
        & HTTP_Version'image (req.version)
        & LF
        & "}";
   end;
end http;
