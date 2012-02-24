;;; Change the path to db2cmd to reflect the correct 
;;; location for your machine. 
(setq sql-db2-program "C:/Program Files/IBM/SQLLIB/BIN/db2cmd.exe") 
 
;;; The interesting options here is the "-t" option 
;;; passed to "db2". This is extremely handy - it 
;;; means that ';' (semicolon) ; is treated as the command 
;;; line terminator. The default is to treat the end-of-line 
;;; as a SQL statement terminator. 
;;; You may look up the command reference online for an 
;;; explanation of the rest. 
(setq sql-db2-options '("-c" "-i" "-w" "db2" "-tv")) 

(setq-default ispell-program-name "aspell")
