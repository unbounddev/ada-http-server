with ada.characters;
with ada.characters.latin_1;
with ada.streams; use ada.streams;
with ada.text_io; use ada.text_io;
with gnat.sockets; use gnat.sockets;
with system;

package http is
   type HTTP_Server is tagged record
      socket: Socket_Type;
      addr: Address_Info;
      conn_socket: Socket_Type;
      client_addr: Sock_Addr_Type;
      port: String(1..5);
   end record;

   procedure listen(self: in out HTTP_Server; port: String);

   type Request_Methods is (
      GET, -- Transfer a current representation of the target resource. (Safe)
      HEAD, -- Same as GET, but do not transfer the response content. (Safe)
      POST, -- Perform resource-specific processing on the request content.
      PUT, -- Replace all current representations of the target resource with the request content.
      DELETE, -- Remove all current representations of the target resource.
      CONNECT, -- Establish a tunnel to the server identified by the target resource.
      OPTIONS, -- Describe the communication options for the target resource. (Safe)
      TRACE -- Perform a message loop-back test along the path to the target resource. (Safe)
   );

   function to_stream_element_array(str: String) return Stream_Element_Array;
   function image(item: Address_Info) return String;

   -- TODO: Parse request line
   -- TODO:  - method
   -- TODO:  - request target (uri)
   -- TODO:  - http version
end;