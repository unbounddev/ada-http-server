package body http is
   procedure listen(self: in out HTTP_Server; port: String) is
      CR: constant Character := ada.characters.latin_1.CR;
      LF: constant Character := ada.characters.latin_1.LF;
      resp: Stream_Element_Array := to_stream_element_array ("HTTP/1.1 200 OK" & CR & LF & CR & LF & "Request received!");
      res: Address_Info_Array := get_address_info(host => "", service => PORT, passive => true);
      data: Stream_Element_Array(1..1024);
      last: Stream_Element_Offset;
      
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
      while True loop
         accept_socket(self.socket, self.conn_socket, self.client_addr);
         put_line("Accepting connection from " & image(self.client_addr));
         -- ready to communicate on socket descriptor self.conn_socket
         receive_socket(self.conn_socket, data, last);
         put_line("" & Character'Val(data(1)));
         send_socket(self.conn_socket, resp, last);
         put_line(Long_Long_Integer'Image(Long_Long_Integer(last)) & " characters sent!");
         put_line("Closing connection to " & image(self.client_addr));
         close_socket(self.conn_socket);
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
         arr(Stream_Element_Offset(c)) := Stream_Element(Character'Pos(str(c)));
      end loop;
      return arr;
   end;
end http;