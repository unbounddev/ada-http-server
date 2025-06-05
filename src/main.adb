with http;

procedure main is
   -- the port users will be connecting to
   PORT: constant String := "3490"; 
   server: http.HTTP_Server;
begin
   server.listen (PORT);
end main;