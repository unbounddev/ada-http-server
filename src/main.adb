with ada.characters;
with ada.characters.latin_1;
with ada.streams; use ada.streams;
with ada.text_io; use ada.text_io;
with gnat.sockets; use gnat.sockets;
with system;

procedure main is
   PORT: constant String := "3490"; -- the port users will be connecting to
   CR: constant Character := ada.characters.latin_1.CR;
   LF: constant Character := ada.characters.latin_1.LF;
   function to_stream_element_array(str: String) return Stream_Element_Array;
   res: Address_Info_Array := get_address_info(host => "", service => PORT, passive => true);
   main_socket: Socket_Type;
   main_addr: Address_Info;
   their_addr: Sock_Addr_Type;
   new_socket: Socket_Type;
   client_addr: Sock_Addr_Type;
   data: Stream_Element_Array(1..1024);
   last: Stream_Element_Offset;
   reuse_option: Option_Type(Reuse_Address) := (name => Reuse_Address, enabled => true);
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
   resp: Stream_Element_Array := to_stream_element_array ("HTTP/1.1 200 OK" & CR & LF & CR & LF & "Request received!");
begin
   if res'length < 1 then put_line(standard_error, "No available address to bind to"); end if;
   main_addr := res(1);
   -- make a socket, bind it, and listen on it
   put_line("Creating socket for " & image(main_addr));
   create_socket(main_socket, main_addr.addr.family, main_addr.mode, main_addr.level);
   --  set_socket_option(main_socket, main_addr.level, reuse_option);
   put_line("Binding socket to " & image(main_addr.addr));
   bind_socket(main_socket, main_addr.addr);
   put_line("Listening on " & image(main_addr.addr));
   listen_socket(main_socket);

   -- now accept an incoming connection
   while True loop
      accept_socket(main_socket, new_socket, client_addr);
      put_line("Accepting connection from " & image(client_addr));
      -- ready to communicate on socket descriptor new_socket
      receive_socket(new_socket, data, last);
      put_line("" & Character'Val(data(1)));
      send_socket(new_socket, resp, last);
      put_line(Long_Long_Integer'Image(Long_Long_Integer(last)) & " characters sent!");
      put_line("Closing connection to " & image(client_addr));
      close_socket(new_socket);
   end loop;
   
end main;