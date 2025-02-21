val tf2Quotes = [
    		"I never really was on your side.\r\n",
    		"Let's go practice medicine.\r\n",
    		"That spy is not one of us!\r\n",
    		"Everyone! I have an announcement to make. We're all going to die.\r\n"
];

val quote_index = ref 0;

fun next_quote () =
    let
        val i = !quote_index
        val quote = List.nth (tf2Quotes, i)
        val _ = quote_index := (i + 1) mod (length tf2Quotes)
    in
        quote
    end;

fun sendQuote sock =
    let
        val res = next_quote ()
        val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in
        Socket.sendVec (sock, slc);
        Socket.close sock
    end;

fun acceptLoop serv =
    let val (s, _) = Socket.accept serv
    in print "Accepted a connection...\n";
       sendQuote s;
       acceptLoop serv
    end

fun serve () =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any 1717);
       Socket.listen(s, 5);
       print "Entering accept loop...\n";
       acceptLoop s
    end

(* 
  For Poly/ML fun main () = serve (
  For SML/NJ val _ = serve ()
  You probably want to use one or the other but not both!
*)

fun main () = serve ()
val _ = main()
