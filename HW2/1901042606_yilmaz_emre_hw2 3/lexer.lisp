(defun checkKeywords (currentList)

    (cond
        ( (string-equal currentList "AND") (string "KW_AND"))
        ( (string-equal currentList "OR") (string "KW_OR" ))
        ( (string-equal currentList "NOT") (string "KW_NOT"))
        ( (string-equal currentList "EQUAL") (string "KW_EQUAL"))
        ( (string-equal currentList "LESS") (string "KW_LESS"))
        ( (string-equal currentList "NIL") (string"KW_NIL"))
        ( (string-equal currentList "LIST") (string "KW_LIST"))
        ( (string-equal currentList "APPEND") (string "KW_APPEND"))
        ( (string-equal currentList "CONCAT") (string "KW_CONCAT")) 
        ( (string-equal currentList "SET") (string "KW_SET")) 
        ( (string-equal currentList "DEFFUN") (string "KW_DEFFUN")) 
        ( (string-equal currentList "FOR") (string "KW_FOR"))
        ( (string-equal currentList "IF") (string "KW_IF")) 
        ( (string-equal currentList "EXIT") (string "KW_EXIT")) 
        ( (string-equal currentList "LOAD") (string "KW_LOAD"))
        ( (string-equal currentList "DISP") (string "KW_DISP"))
        ( (string-equal currentList "TRUE") (string "KW_TRUE"))
        ( (string-equal currentList "FALSE") (string "KW_FALSE")) 
        ( t (string "nothing"))
    )
)

(defun checkOperator (str)
    (cond
        ((string-equal str "+") (string "OP_PLUS"))
        ((string-equal str "-") (string "OP_MINUS"))
        ((string-equal str "*") (string "OP_MULT"))
        ((string-equal str "/") (string "OP_DIV"))
        ((string-equal str "(") (string "OP_OP"))
        ((string-equal str ")") (string "OP_CP"))
        ((string-equal str "**") (string "OP_DBMULT"))
        ((string-equal str ",") (string "OP_COMMA"))
        ((string-equal str ":;") (string "COMMENT"))
        ( t (string "nothing"))
    )
)

(defun checkDigit (str)
    (cond
        ((string-equal str "0") (setq a #\0))
        ((string-equal str "1") (setq a #\1))
        ((string-equal str "2") (setq a #\2))
        ((string-equal str "3") (setq a #\3))
        ((string-equal str "4") (setq a #\4))
        ((string-equal str "5") (setq a #\5))
        ((string-equal str "6") (setq a #\6))
        ((string-equal str "7") (setq a #\7))
        ((string-equal str "8") (setq a #\8))
        ((string-equal str "9") (setq a #\9))
        ( t (string "nothing"))
    )
)

(defun iterateInput (inputList currentList idFlag numberFlag fnumberflag strflag outputlist )
    (cond
    ( (equal (length inputList) 0)
        (setq currentChar (car inputList))
        (setq keywordStatus (checkkeywords (coerce currentlist 'string)))
        (cond
            ( (= idFlag 1)
            (setq output "IDENTIFIER")

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))
            (return-from iterateInput outputlist)
            )

            ( (= numberFlag 1)
            (setq output "VALUEI")
            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)
            (return-from iterateInput outputlist)

            )

            ( (= fnumberFlag 1)

            (setq output "VALUEF")

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (return-from iterateInput outputlist)
            )

            ( (string-not-equal keywordStatus "nothing")

            (princ keywordstatus)
            (princ " ")
            (princ (coerce currentlist 'string))
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons keywordstatus lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (return-from iterateInput outputlist)
            )

            ( t
            (return-from iterateinput outputlist)
            )
        
        )
    )
    )
    
    ; read 1 char, append to list
    (setq currentChar (car inputList))
    (setq inputList (cdr inputList))
    (setq str (string currentChar ))

    
    (setq operatorStatus (checkoperator currentChar )) ; see the char is operator or not
    (setq digitstatus (checkdigit currentChar )) ; see the char is a digit or not
    
    (cond
        ( (char= currentChar #\" ) ; if we read "

            (cond
                ( (equal strflag 1) ; if we are currently reading a string
                (setq strFlag 0)
                (setq output "VALUESTR")
                (setq currentlist (append currentlist (list currentchar)))

                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)
                
                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))

                (setq strflag 0 )
                (setq currentList '()) ; make the current list empty
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )
                ( t ; if we are NOT currently reading a string
                (setq outputlist (seperator inputList currentList idFlag numberFlag fnumberflag strflag outputlist))
                (setq currentList '()) ; make the current list empty
                (setq strflag 1)
                (setq idflag 0)
                (setq numberflag 0)
                (setq fnumberflag 0)
                (setq currentlist (append currentlist (list currentchar)))
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )
            )
        )

        ( (equal currentchar #\*) ; if we read STAR
            (setq outputlist (seperator inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            (setq currentList '()) ; make the current list empty
            (setq newChar (car inputList))
            
            (setq idflag 0)
            (setq numberflag 0)
            (setq fnumberflag 0)

            (cond
                ( (equal newchar #\*) ; if we read one more STAR, it means that we read DBL_MULT
                (setq output "OP_DBLMLT")
                (setq currentlist (append currentlist (list currentchar)))
                (setq currentlist (append currentlist (list newchar)))
                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)

                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))
                (setq currentList '()) ; make the current list empty

                (setq inputList (cdr inputList))
                (setq currentList '()) ; make the current list empty
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )

                ( t ; if we do NOT read one more star it means that we must print OP_MULTT
                (setq output "OP_MULT")
                (setq currentlist (append currentlist (list currentchar)))
                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)

                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))
                (setq currentList '()) ; make the current list empty
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )
            )
            
        )

        ( (equal strflag 1) ;  if we are currently reading a string, it means that we have to add the next char to string
        (setq currentlist (append currentlist (list currentchar)))
        (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
        )
        ( (equal currentchar #\:) ; if we read the : character
            (setq outputlist (seperator inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            (setq currentList '()) ; make the current list empty
            (setq newChar (car inputList))
            
            (setq idflag 0)
            (setq numberflag 0)
            (setq fnumberflag 0)

            (cond
                ( (equal newchar #\;) ; if we read ; after :, it means that we have read a comment operator
                (setq output "COMMENT")
                (setq currentlist (append currentlist (list currentchar)))
                (setq currentlist (append currentlist (list newchar)))
                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)

                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))
                (setq currentList '()) ; make the current list empty

                (setq inputList (cdr inputList))
                (setq currentList '()) ; make the current list empty
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )

                ( t
                (setq output "ERROR")
                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)
                )
            )
            
        )

        ; CONDITION 1 ( operator durumu)
        ( (string-not-equal operatorStatus "nothing") 
        (setq keywordStatus (checkkeywords (coerce currentlist 'string)))
        (cond 
            ; INNER CONDITION 1
            ( (string-not-equal keywordStatus "nothing") ; if the current list is keyword

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ keywordStatus)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons keywordstatus lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (setq currentList (append currentList (list currentchar)))

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ operatorstatus)
            (princ #\linefeed)

            (setq currentList '()) ; make the current list empty

            (setq numberFlag 0)
            (setq idFlag 0)
            (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )

            ; INNER CONDITION 2
            ( (equal idFlag 1)
            ; INNER ACTION 2

            (setq output "IDENTIFIER")
            (setq numberFlag 0)
            (setq idFlag 0)
            (setq fnumberFlag 0)

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (princ currentchar)
            (princ " ")
            (princ operatorstatus)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq lexemetoken '())
            (setq lexemetoken (cons operatorstatus lexemetoken))
            (setq lexemetoken (cons currentChar lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )

            ( (equal numberFlag 1)
            ; INNER ACTION 2
            (setq numberFlag 0)
            (setq idFlag 0)
            (setq output "VALUEI")

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (princ currentchar)
            (princ " ")
            (princ operatorstatus)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq lexemetoken '())
            (setq lexemetoken (cons operatorstatus lexemetoken))
            (setq lexemetoken (cons currentChar lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )

            ( (equal fnumberFlag 1)
            ; INNER ACTION 2
            (setq output "VALUEF")
            (setq numberFlag 0)
            (setq fnumberFlag 0)
            (setq idFlag 0)

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (princ currentchar)
            (princ " ")
            (princ operatorstatus)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq lexemetoken '())
            (setq lexemetoken (cons operatorstatus lexemetoken))
            (setq lexemetoken (cons currentChar lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))
            
            (setq currentList '()) ; make the current list empty
            (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )

            (t
            (setq output operatorStatus)
            (princ currentchar)
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons operatorstatus lexemetoken))
            (setq lexemetoken (cons currentChar lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )
        )
        )

        ; CONDITION 2 ( digit durumu )
        ( (string-not-equal digitStatus "nothing")
            
            (cond
                ( (equal (length currentList) 0)
                ;(setq currentList (append currentList "VALUEI "))
                (setq idFlag 0)
                (setq numberFlag 1)
                (setq fnumberFlag 0)
                (setq currentList (append currentlist (list currentchar)))
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )

                ; INNER CONDITION 2
                ((equal idFlag 1)
                (setq currentList (append currentList (list digitStatus)))
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )

                ; INNER CONDITION 2
                ( t
                
                (cond
                    ( (equal fnumberFlag 1)
                    (setq idFlag 0)
                    (setq currentList (append currentlist (list currentchar)))
                    (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                    )

                    ( (equal numberFlag 0)
                    (setq numberFlag 1)
                    (setq idFlag 0)
                    (setq keywordStatus (checkkeywords (coerce currentlist 'string)))
                    
                    (cond 
                        ( (string-not-equal keywordStatus "nothing") 
                        (setq currentList (append currentList operatorStatus))

                        (princ (coerce currentlist 'string))
                        (princ " ")
                        (princ keywordStatus)

                        (setq lexemetoken '())
                        (setq lexemetoken (cons keywordStatus lexemetoken))
                        (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                        (setq outputlist (cons lexemetoken outputlist))

                        (setq currentList '()) ; make the current list empty
                        (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                        )

                        (t
                        (setq output "ERROR")
                        (princ (coerce currentlist 'string))
                        (princ " ")
                        (princ output)
                        (princ #\linefeed)
                        )
                    )
                    )

                    ( t

                    (setq numberFlag 1)
                    (setq idFlag 0)
                    (setq currentList (append currentlist (list currentchar)))
                    (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                    )
                )
                )
            )
        )

        ; char okuma case'i
        ( (alpha-char-p currentChar)
        (cond
            ( (equal idFlag 1)
            (setq currentList (append currentList (list currentChar)))
            (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )

            ( (equal numberFlag 1)
            (setq newChar (car inputList))
            (setq digitstatus (checkdigit newChar ))
            (cond
                ( (and (char= #\f currentChar) (string-not-equal digitstatus "nothing") )
                (setq numberflag 0)
                (setq fnumberflag 1)
                (setq inputList (cdr inputList))
                (setq currentList (append currentList (list currentChar)))
                (setq currentList (append currentList (list newchar)))
                
                (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist )
                )

                ( t
                    (setq output "VALUEI")
                    (princ (coerce currentlist 'string))
                    (princ " ")
                    (princ output)
                    (princ #\linefeed)

                    (setq idFlag 1)
                    (setq numberFlag 0)
                    
                    (setq lexemetoken '())
                    (setq lexemetoken (cons output lexemetoken))
                    (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                    (setq outputlist (cons lexemetoken outputlist))
                    
                    (setq currentList '()) ; make the current list empty
                    (setq currentList (append currentList (list currentChar)))
                    (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )
            )
            )

            ( t
            ()
                (setq idFlag 1)
                (setq currentList (append currentList (list currentChar)))
                (return-from iterateInput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
            )
        )
        )
    ; bosluk okuma case'i
    ( (char= currentChar #\Space )
    (setq keywordStatus (checkkeywords (coerce currentlist 'string)))
    (cond
        ( (string-not-equal keywordStatus "nothing") ; if the current list is keyword

        (princ (coerce currentlist 'string))
        (princ " ")
        (princ keywordStatus)
        (princ #\linefeed)

        (setq lexemetoken '())
        (setq lexemetoken (cons keywordStatus lexemetoken))
        (setq lexemetoken (cons ( coerce currentlist 'string) lexemetoken))
        (setq outputlist (cons lexemetoken outputlist))
        

        (setq numberFlag 0)
        (setq idFlag 0)

        (setq currentList '()) ; make the current list empty
        (return-from iterateinput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
        )

        ( (equal idFlag 1) ; if the current list is identifier
            (setq output "IDENTIFIER")
            (setq idFlag 0)
            (setq numberFlag 0)
            
            (princ ( coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons ( coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (return-from iterateinput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
        )

        ( (equal numberFlag 1) ; if the current list is VALUEI
            (setq output "VALUEI")
            (setq idFlag 0)
            (setq numberFlag 0)

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)
            
            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (return-from iterateinput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
        )

        ( (equal fnumberFlag 1) ; if the current list is VALUEF
            (setq output "VALUEF")
            (setq idFlag 0)
            (setq numberFlag 0)
            (setq fnumberFlag 0)

            (princ (coerce currentlist 'string))
            (princ " ")
            (princ output)
            (princ #\linefeed)

            (setq lexemetoken '())
            (setq lexemetoken (cons output lexemetoken))
            (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
            (setq outputlist (cons lexemetoken outputlist))

            (setq currentList '()) ; make the current list empty
            (return-from iterateinput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
        )

        ( t ; if an error occurs
            ;(setq garbage '() )
            (cond
                ( (equal currentList '() ) 
                    (return-from iterateinput (iterateinput inputList currentList idFlag numberFlag fnumberflag strflag outputlist ))
                )

                ( t
                    (setq output "ERROR")
                    (princ (coerce currentlist 'string))
                    (princ " ")
                    (princ output)
                    (princ #\linefeed)
 
                )
            )
        )
    )
    )
        
    ; error basma case'i
    (   t
        (setq outputlist (seperator inputList currentList idFlag numberFlag fnumberflag strflag outputlist))
        (setq output "ERROR")
        (princ currentchar)
        (princ " ")
        (princ output)
        (princ #\linefeed)

        (return-from iterateInput outputlist)
    )
    )
)

(defun seperator (inputList currentList idFlag numberFlag fnumberflag strflag outputlist ) ; this function used 2 times, when we read a seperator char, this function runs.
    (setq keywordStatus (checkkeywords (coerce currentlist 'string)))
        (cond
            ( (string-not-equal keywordStatus "nothing") 
            (setq currentList '()) ; make the current list empty
            (princ keywordStatus)
            (setq numberFlag 0)
            (setq idFlag 0)
            )

            ( (equal idFlag 1) 
                (setq output "IDENTIFIER")
                (setq idFlag 0)
                (setq numberFlag 0)
                
                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)

                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))

                (setq currentList '()) ; make the current list emptykeywordStatus
            )

            ( (equal numberFlag 1)
                (setq output "VALUEI")
                (setq idFlag 0)
                (setq numberFlag 0)

                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)

                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))

                (setq currentList '()) ; make the current list empty
            )

            ( (equal fnumberFlag 1) 
                (setq output "VALUEF")
                (setq idFlag 0)
                (setq numberFlag 0)
                (setq fnumberFlag 0)

                (princ (coerce currentlist 'string))
                (princ " ")
                (princ output)
                (princ #\linefeed)

                (setq lexemetoken '())
                (setq lexemetoken (cons output lexemetoken))
                (setq lexemetoken (cons (coerce currentlist 'string) lexemetoken))
                (setq outputlist (cons lexemetoken outputlist))

                (setq currentList '()) ; make the current list empty
            )

            ( t
                ;(setq garbage '() )
                (cond
                    ( (equal currentList '() ) 
                    )

                    ( t
                        (setq output "ERROR")
                        (princ (coerce currentlist 'string))
                        (princ " ")
                        (princ output)
                        (princ #\linefeed)
                    )
                )
            )
        )
    (return-from seperator outputlist)
)       
    
(defun readfromtext (filestatus)

    (setq input (read-line filestatus nil))
    (setq inputList (coerce input 'list)) ; make the input a list
    (setq currentList '()) ; make the current list empty
    (setq outputlist '()) ; make the current list empty
    (setq idFlag 0)
    (setq numberFlag 0)
    (setq fnumberFlag 0)
    (setq strflag 0)
    (princ #\linefeed)
    
    (if (equal input nil)
        (return-from readfromtext (gppinterpreter nil))
    )
    (setq outputList (iterateInput inputList currentList idFlag numberFlag fnumberFlag strflag outputlist))

    (princ #\linefeed)
    (princ "The line's final list is: ")
    (princ #\linefeed)
    (setq outputlist (reverse outputlist))
    (princ outputlist)
    (princ #\linefeed)
    (return-from readfromtext (readfromtext filestatus))

)

(defun gppinterpreter (filename)
    
    (setq filestatus nil)

    (cond
    (   (not (null filename))
        (setq filestatus (open filename))
        (princ "File is being readed...")
        (princ #\linefeed)
        (readfromtext filestatus)
    )
    
    ( t
    
    (princ #\linefeed)
    (princ "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
    (princ #\linefeed)
    (princ "Please enter the line, the final output list ,which will be created and printed, will only include entered lexemes in a line. If you want multiple-lines, please use a file")
    (princ #\linefeed)
    (princ "Please enter line: ")
    (setq input (read-line)) ; Get the all input and assign to a variable
    (setq inputList (coerce input 'list)) ; make the input a list
    (setq currentList '()) ; make the current list empty
    (setq outputlist '()) ; make the current list empty
    (setq idFlag 0)
    (setq numberFlag 0)
    (setq fnumberFlag 0)
    (setq strflag 0)
    (princ #\linefeed)
    
    (setq outputList (iterateInput inputList currentList idFlag numberFlag fnumberFlag strflag outputlist))

    (princ #\linefeed)
    (princ "The final list is: ")
    (setq outputlist (reverse outputlist))
    (princ outputlist)
    (princ #\linefeed)
    (gppinterpreter filename)
    )
))

(setq filename (car *args*))
(gppinterpreter filename)