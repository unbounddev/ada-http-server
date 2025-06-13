with http;

procedure main is
   -- the port users will be connecting to
   PORT: constant String := "3491"; 
   server: http.HTTP_Server;
begin
   server.listen (PORT);
end main;