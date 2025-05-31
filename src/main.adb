with Ada.Characters;
with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ada.text_io; use ada.text_io;
with gnat.sockets; use gnat.sockets;
with System;

procedure main is
   PORT: constant string := "3490"; -- the port users will be connecting to
   BACKLOG: constant Natural := 10; -- how many pending connections queue will hold
   CR: constant Character := Ada.Characters.Latin_1.CR;
   LF: constant Character := Ada.Characters.Latin_1.LF;
   function Stream_Element_Array(str: String) return Ada.Streams.Stream_Element_Array;
   res: Address_Info_Array := get_address_info(host => "", service => PORT, passive => true);
   sockfd: Socket_Type;
   new_fd: Socket_Type;
   first_res: Address_Info;
   their_addr: Sock_Addr_Type;
   reuse_option: Option_Type(Reuse_Address) := (name => Reuse_Address, enabled => true);
   function image(item: Address_Info) return String is
   begin
      return "family: " & Family_Type'image(item.addr.family) & " mode: " & Mode_Type'image(item.mode) & " level: " & Level_Type'image(item.level); 
   end image;

   function Stream_Element_Array(str: String) return Ada.Streams.Stream_Element_Array is
      arr: Ada.Streams.Stream_Element_Array (1..str'length);
   begin
      for c in str'range loop
         arr(Ada.Streams.Stream_Element_Offset(c)) := Ada.Streams.Stream_Element(Character'Pos(str(c)));
      end loop;
      return arr;
   end;
   resp: Ada.Streams.Stream_Element_Array := Stream_Element_Array ("HTTP/1.1 200 OK" & CR & LF & CR & LF & "Request received!");
begin
   if res'length < 1 then put_line(standard_error, "No available address to bind to"); end if;
   first_res := res(1);
   -- make a socket

   -- make a socket, bind it, and listen on it
   put_line("Creating socket for " & image(first_res));
   create_socket(sockfd, first_res.addr.family, first_res.mode, first_res.level);
   --  set_socket_option(sockfd, first_res.level, reuse_option);
   put_line("Binding socket to " & image(first_res.addr));
   bind_socket(sockfd, first_res.addr);
   put_line("Listening on " & image(first_res.addr));
   listen_socket(sockfd, BACKLOG);

   -- now accept an incoming connection
   while True loop
      declare
         new_socket: Socket_Type;
         client_addr: Sock_Addr_Type;
         data: Ada.Streams.Stream_Element_Array(1..1024);
         last: Ada.Streams.Stream_Element_Offset;
      begin
         accept_socket(sockfd, new_socket, client_addr);
         put_line("Accepting connection from " & image(client_addr));
         -- ready to communicate on socket descriptor new_fd
         --  Send_Socket (new_fd, Item => Ada.Streams.Stream_Element_Array, Last => out Ada.Streams.Stream_Element_Offset, Flags => Request_Flag_Type)
         receive_socket(new_socket, data, last);
         put_line("" & Character'Val(data(1)));
         send_socket(new_socket, resp, last);
         put_line(Long_Long_Integer'Image(Long_Long_Integer(last)) & " characters sent!");
         put_line("Closing connection to " & image(client_addr));
         close_socket(new_socket);
      end;
   end loop;
   
end main;