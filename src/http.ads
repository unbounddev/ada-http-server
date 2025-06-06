with ada.characters;
with ada.characters.latin_1;
with ada.streams; use ada.streams;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
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

   package String_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   procedure listen(self: in out HTTP_Server; port: String);

   Bad_Request: exception;
   Unsafe_Character: exception;
   Bad_Method: exception;

   type ASCII is range 16#0# .. 16#7f#;

   SPACE: constant Stream_Element := Character'pos(ada.characters.latin_1.Space);
   CR: constant Stream_Element := Character'pos(ada.characters.latin_1.CR);
   LF: constant Stream_Element := Character'pos(ada.characters.latin_1.LF);

   type Request_Method is (
      GET, -- Transfer a current representation of the target resource. (Safe)
      HEAD, -- Same as GET, but do not transfer the response content. (Safe)
      POST, -- Perform resource-specific processing on the request content.
      PUT, -- Replace all current representations of the target resource with the request content.
      DELETE, -- Remove all current representations of the target resource.
      CONNECT, -- Establish a tunnel to the server identified by the target resource.
      OPTIONS, -- Describe the communication options for the target resource. (Safe)
      TRACE -- Perform a message loop-back test along the path to the target resource. (Safe)
   );

   type HTTP_Version is (
      --  HTTP_0_9,
      HTTP_1_0,
      HTTP_1_1
      --  HTTP_2,
      --  HTTP_3
   );

   type HTTP_Request is tagged record
      method: Request_Method;
      uri: Ada.Strings.Unbounded.Unbounded_String;
      version: HTTP_Version;
      headers: String_Hashed_Maps.Map;
   end record;

   procedure parse_request(req: Stream_Element_Array);
   function parse_request_method(req: Stream_Element_Array; last: in out Stream_Element_Offset) return Request_Method;
   function parse_request_uri(req: Stream_Element_Array; last: in out Stream_Element_Offset) return Ada.Strings.Unbounded.Unbounded_String;
   function parse_http_version(req: Stream_Element_Array; last: in out Stream_Element_Offset) return HTTP_Version;
   --  function parse_request_headers(req: Stream_Element_Array; last: in out Stream_Element_Offset) return String_Hashed_Maps.Map;
   procedure parse_character(req: Stream_Element_Array; last: in out Stream_Element_Offset; ch: Stream_Element);
   function is_ascii(e: Stream_Element) return Boolean;
   function to_stream_element_array(str: String) return Stream_Element_Array;
   function image(item: Address_Info) return String;
   function image(req: HTTP_Request) return String;
end;