;(deff emre () (((+ 3 2)))
;(require "CL")



;(deffun emre () (((+ 3 2)))

(defparameter counter 0)
(setq idlist (list))
(setq funcList (list))

(defun get-prefix (input separator)
  (let ((prefix (subseq input 0 (position separator input))))
    (read (make-string-input-stream prefix))
    ))

(defun get-postfix (string separator)
  (let ((index (position separator string)))
    (if index
        (read-from-string(subseq string (1+ index)))
      nil)))


(defun checkFuncList (element list)
  (if (null list)
      nil
      (if (equal element (car list))
          t
          (checkFuncList element (cdr list)))))

(defun get-value (key alist)
  (cond
    ((null alist) nil)
    ((string-equal key (car (car alist))) (return-from get-value (cdr (car alist))))
    (t (get-value key (cdr alist)))))


(defun addIdList (key value alist)
  (cons (cons key value) alist))

(defun checkIdExist (key alist)
  (cond
    ((null alist) nil)
    ((string-equal key (car (car alist))) t)
    (t (checkIdExist key (cdr alist))))
    )

(defun updateId (key new-value alist)
  (cond
    ((null alist) nil)
    ((eq key (car (car alist))) (cons (cons key new-value) (cdr alist)))
    (t (cons (car alist) (updateId key new-value (cdr alist))))))



    
;;This function determines the operator token
(defun checkOperatorParse (token)
	(return-from checkoperatorparse (or (string-equal "OP_PLUS" token)
		(string-equal "OP_MINUS" token)
		(string-equal "OP_DIV" token)
		(string-equal "OP_MULT" token)	
	))
)

(defun calculateArithmetic (num1 num2 operator)


(cond
    (   (string-equal "OP_PLUS" operator)
        ; (princ "Syntax is OK ")
        ; (princ (+ num1 num2))
        (return-from calculatearithmetic (+ num1 num2))
    )

    (   (string-equal "OP_MINUS" operator)
        ; (princ "Syntax is OK ")
        ; (princ (- num1 num2))
        (return-from calculatearithmetic (- num1 num2))
    )

    (   (string-equal "OP_MULT" operator)
        ; (princ "Syntax is OK ")
        ; (princ (* num1 num2))
        (return-from calculatearithmetic (* num1 num2))
    )

    (   (string-equal "OP_DIV" operator)
        ; (princ "Syntax is OK ")
        ; (princ (/ num1 num2))
        (return-from calculatearithmetic (/ num1 num2))
    )
)

)

(defun checkIfFunc (tokenlist)

(cond
        (   (and (string-equal "OP_OP" (nth 1 (nth (+ counter 0) tokenlist))) (string-equal "deffun" (nth 0 (nth (+ counter 1) tokenlist))) 
            (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 2) tokenlist))) (string-equal "OP_OP" (nth 1 (nth (+ counter 3) tokenlist))))

        

            (setq funclist (cons  (nth 0 (nth (+ counter 2) tokenlist)) funclist))
            (defparameter counter (+ counter 4))
            
            (cond
                (   (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                    (if (string-equal "OP_OP" (nth 1 (nth (+ counter 1) tokenlist)))
                        (progn
                        
                            (defparameter counter (+ counter 2))
                            (setq result (checkifexplist tokenlist))
                            (if (and (or (eq (type-of result) 'bit) (eq (type-of result) 'ratio) (equal (car (type-of result)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                                (progn
                                    (defparameter counter (+ counter 1))
                                (RETURN-from checkiffunc 1)
                                )
                                
                            )
                        )
                    )
                    (RETURN-from checkiffunc "false")
                )

                (   (and (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 0) tokenlist))) 
                        (string-equal "OP_CP" (nth 1 (nth (+ counter 1) tokenlist))) )

                    ; (if (not (checkidexist (nth 0 (nth (+ counter 0) tokenlist)) idlist))
                    ;     (progn
                    ;         (princ "There is no such identifier: ")
                    ;         (princ (nth 1 (nth (+ counter 0) tokenlist)))
                    ;         (return-from checkiffunc "false")
                    ;     )
                    ; )


                    (if (string-equal "OP_OP" (nth 1 (nth (+ counter 2) tokenlist)))
                        (progn
                            (defparameter counter (+ counter 3))
                            (setq result (checkifexplist tokenlist))
                            (if (and (or (eq (type-of result) 'bit) (eq (type-of result) 'ratio) (equal (car (type-of result)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                                (progn
                                    (defparameter counter (+ counter 1))
                                (RETURN-from checkiffunc 1)
                                )
                            )
                        )
                    )
                    (RETURN-from checkiffunc "false")
                )

                (   (and (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 0) tokenlist))) (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 1) tokenlist))) 
                        (string-equal "OP_CP" (nth 1 (nth (+ counter 2) tokenlist))) )


                    ; (if (not (checkidexist (nth 0 (nth (+ counter 0) tokenlist)) idlist))
                    ;     (progn
                    ;         (princ "There is no such identifier: ")
                    ;         (princ (nth 1 (nth (+ counter 0) tokenlist)))
                    ;         (return-from checkiffunc "false")
                    ;     )
                    ; )

                    (if (string-equal "OP_OP" (nth 1 (nth (+ counter 3) tokenlist)))
                        (progn
                            (defparameter counter (+ counter 4))
                            (setq result ( checkifexplist tokenlist))
                            (if (and (or (eq (type-of result) 'bit) (eq (type-of result) 'ratio) (equal (car (type-of result)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                                (progn
                                    (defparameter counter (+ counter 1))
                                (RETURN-from checkiffunc 1)
                                )
                            )
                        )
                    )
                    (RETURN-from checkiffunc "false")
                )

                (   (and (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 0) tokenlist))) (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 1) tokenlist))) (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 2) tokenlist))) 
                        (string-equal "OP_CP" (nth 1 (nth (+ counter 3) tokenlist))) )

                    ; (if (not (checkidexist (nth 0 (nth (+ counter 0) tokenlist)) idlist))
                    ;     (progn
                    ;         (princ "There is no such identifier: ")
                    ;         (princ (nth 1 (nth (+ counter 0) tokenlist)))
                    ;         (return-from checkiffunc "false")
                    ;     )
                    ; )

                    (if (string-equal "OP_OP" (nth 1 (nth (+ counter 4) tokenlist)))
                        (progn
                            (defparameter counter (+ counter 5))
                            (setq result (checkifexplist tokenlist))
                            (if (and (or (eq (type-of result) 'bit) (eq (type-of result) 'ratio) (equal (car (type-of result)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                                (progn
                                    (defparameter counter (+ counter 1))
                                (RETURN-from checkiffunc 1)
                                )
                            )
                        )
                    )
                    (RETURN-from checkiffunc "false")

                    (t
                    (RETURN-from checkiffunc "false")
                    )
                    
                )
            
            )
        )
)

(RETURN-from checkiffunc "false")


)



(defun checkIfAsg (tokenList)
    (cond
        ((and (string-equal "set" (nth 0 (nth (+ counter 0) tokenlist))) (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 1) tokenlist))) )
            
                (setq idd (nth 1 (nth (+ counter 1) tokenlist)))
                (setq idv (nth 0 (nth (+ counter 1) tokenlist)))

                        
                        (if (string-equal "IDENTIFIER" idd)
                            (progn
                            
                                (defparameter counter (+ counter 2))

                                (cond
                                    ( (checkidexist idd idlist)

                                        (setq exp1 (checkifexp tokenlist))
                                        (if (and (or (eq (type-of exp1) 'bit) (eq (type-of exp1) 'ratio) (equal (car (type-of exp1)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))) )
                                            (progn
                                            (setq idlist (updateid idd exp1 idlist))
                                            (defparameter counter (+ counter 1))
                                            )
                                            
                                        )
                                    )

                                    (t
                                        (setq exp1 (checkifexp tokenlist))
                                        (if (and (or (eq (type-of exp1) 'bit) (eq (type-of exp1) 'ratio) (equal (car (type-of exp1)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))) )
                                            (progn
                                            (setq idlist (addIdList idv exp1 idlist))
                                            (defparameter counter (+ counter 1))
                                            )
                                            
                                        )
                                    )
                                )
                            )
                        )
        )
    )
)

(defun checkIfFcall (tokenList)
    
    (cond
        (   (and (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 0) tokenlist))) )
            

            ; (if (not (member (nth 0 (nth (+ counter 0) tokenlist)) funclist))
            ;     (progn
            ;         (princ "There is no such function: ")
            ;         (princ (nth 0 (nth (+ counter 0) tokenlist)))
            ;         (return-from checkIfFcall "false")
            ;     )
            ;     )

            (if (not (checkFuncList (nth 0 (nth (+ counter 0) tokenlist)) funclist))
             (progn
                  (princ "There is no such function: ")
                   (princ (nth 0 (nth (+ counter 0) tokenlist)))
                   (princ #\newline)
                   (return-from checkIfFcall "false")
                
                 ))

                (defparameter counter (+ counter 1))

            ; (if (not (checkidexist (nth 0 (nth (+ counter 0) tokenlist)) idlist))
            ;     (progn
            ;         (princ "There is no such identifier: ")
            ;         (princ (nth 1 (nth (+ counter 0) tokenlist)))
            ;         (return-from checkIfFcall "false")
            ;     )
            ; )
            
            (cond
                ( (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                ;   (princ "Syntax is OK!\n")
                (defparameter counter (+ counter 1))
                  (return-from checkiffcall 1)
                )

                ( (and (checkIfExp tokenList) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                ;   (princ "Syntax is OK!\n")
                (defparameter counter (+ counter 1))
                  (return-from checkiffcall 1)
                )

                ( (and (checkIfExp tokenList) (checkIfExp tokenList) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                ;   (princ "Syntax is OK!\n")
                (defparameter counter (+ counter 1))
                  (return-from checkiffcall 1)
                )

                ( (and (checkIfExp tokenList) (checkIfExp tokenList) (checkIfExp tokenList) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                ;   (princ "Syntax is OK!\n")
                (defparameter counter (+ counter 1))
                  (return-from checkiffcall 1)
                )

                ( t
                    (return-from checkIfFcall "false")
                )
            )
        )
    )
)


(defun checkIfExpb (tokenList)

(cond
            (    (string-equal "KW_TRUE" (nth 1 (nth (+ counter 0) tokenlist)))
                (defparameter counter (+ counter 1))
                (return-from t)
            )

            (   (string-equal "KW_FALSE" (nth 1 (nth (+ counter 0) tokenlist)))
                (defparameter counter (+ counter 1))
                (return-from nil)
            )

            ( (string-equal "OP_OP" (nth 1 (nth (+ counter 0) tokenlist)))
                (defparameter counter (+ counter 1))

                (cond
                    ( (string-equal "gt" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))

                        (let ((num1 (checkIfExp tokenlist)) (num2 (checkifexp tokenlist)))

                                (if (and (or (eq (type-of num1) 'bit) (eq (type-of num1) 'ratio) (equal (car (type-of num1)) 'integer)) 
                                            (or (eq (type-of num2) 'bit) (eq (type-of num2) 'ratio) (equal (car (type-of num2)) 'integer)))

                                    (cond 
                                        (    (and (or (equal (type-of num1) 'bit) (equal (type-of num1) 'ratio) (equal (car (type-of num1)) 'integer))  (or (equal (type-of num2) 'bit) (equal (type-of num2) 'ratio) (equal (car (type-of num2)) 'integer)))
                                            (if (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                                                (progn
                                                    (defparameter counter (+ counter 1))
                                                    (return-from checkIfExpb (> num1 num2))
                                                )
                                                
                                            )
                                        )
                                    
                                    )
                                )
                                (return-from checkIfExpb "false")
                        )
                    )

                    ( (string-equal "equal" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))
                        (let ((num1 (checkIfExp tokenlist)) (num2 (checkifexp tokenlist)))
                                
                                (if (and (or (eq (type-of num1) 'bit) (eq (type-of num1) 'ratio) (equal (car (type-of num1)) 'integer)) 
                                            (or (eq (type-of num2) 'bit) (eq (type-of num2) 'ratio) (equal (car (type-of num2)) 'integer)))

                                    (cond 
                                        (    (and (or (equal (type-of num1) 'bit) (equal (type-of num1) 'ratio) (equal (car (type-of num1)) 'integer))  (or (equal (type-of num2) 'bit) (equal (type-of num2) 'ratio) (equal (car (type-of num2)) 'integer)))
                                            (if (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                                                (progn
                                                    (defparameter counter (+ counter 1))
                                                    (return-from checkIfExpb (= num1 num2))
                                                )
                                                
                                            )
                                        )
                                    
                                    )
                                )
                                (return-from checkIfExpb "false")
                        )
                    )

                    ( (string-equal "and" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))

                        (let ((res1 (checkIfExpb tokenlist)) (res2 (checkifexpb tokenlist)))

                                (if (and (or (eq (type-of res1) 'bit) (eq (type-of res1) 'ratio) (equal (car (type-of res1)) 'integer)) 
                                            (or (eq (type-of res2) 'bit) (eq (type-of res2) 'ratio) (equal (car (type-of res2)) 'integer)))
                                        
                                    (if (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                                                (progn
                                                    (defparameter counter (+ counter 1))
                                                    (return-from checkIfExpb (and res1 res2))
                                                )
                                                
                                            )
                                    (return-from checkIfExpb "false")
                                )
                        )
                    )

                    ( (string-equal "or" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))

                        (let ((res1 (checkIfExpb tokenlist)) (res2 (checkifexpb tokenlist)))

                                (if (and (or (eq (type-of res1) 'bit) (eq (type-of res1) 'ratio) (equal (car (type-of res1)) 'integer)) 
                                            (or (eq (type-of res2) 'bit) (eq (type-of res2) 'ratio) (equal (car (type-of res2)) 'integer)))
                                        
                                    (if (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                                                (progn
                                                    (defparameter counter (+ counter 1))
                                                    (return-from checkIfExpb (or res1 res2))
                                                )
                                                
                                            )
                                    (return-from checkIfExpb "false")
                                )
                        )
                    )

                    ( (string-equal "not" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))

                        (let ((res1 (checkIfExpb tokenlist)) )
                                (if (or (eq (type-of res1) 'bit) (eq (type-of res1) 'ratio) (equal (car (type-of res1)) 'integer))
                                        
                                    (if (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                                                (progn
                                                    (defparameter counter (+ counter 1))
                                                    (return-from checkIfExpb (not res1))
                                                )
                                                
                                            )
                                )
                                (return-from checkIfExpb "false")
                        )
                    )

                    ( t
                        (return-from checkIfExpb "false")
                    )

                )
            )

            ( t
            (return-from checkifexpb "false")

            )
)
)

(defun checkIfExpList (tokenList)
    
    (cond
        ( (and (string-equal "OP_OP" (nth 1 (nth (+ counter 0) tokenlist))))
            (defparameter counter (+ counter 1))
            (return-from checkIfExpList (checkifexplisthelper tokenlist))
        )

        ( t
        
            (return-from checkifexplist "false")
        )
    )
)

(defun checkIfExplistHelper (tokenList)

    (let ((number1 (checkIfExp tokenlist)))
        (if (and (or (eq (type-of number1) 'bit) (eq (type-of number1) 'ratio) (equal (car (type-of number1)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
            (progn
            
                (defparameter counter (+ counter 1))
                (cond 
                    (    (and (or (equal (type-of number1) 'bit)(equal (type-of number1) 'ratio) (equal (car (type-of number1)) 'integer)))
                        ;(if (string-equal "OP_CP" (nth 1 (nth (+ counter 1) tokenlist)))
                            (return-from checkIfExplistHelper number1)
                        ;)
                    )
                )
            )
            
        )

        (if (and (or (eq (type-of number1) 'bit) (eq (type-of number1) 'ratio) (equal (car (type-of number1)) 'integer)) )
            (return-from checkIfExplistHelper (checkIfExpListHelper tokenList))
        )

        (return-from checkIfExplistHelper "false")
    )
)

;(if (eq 3 2) ((+ 2 3)) ((- 3 8)))

(defun checkIfExp (tokenList)

    (cond
            (    (string-equal "IDENTIFIER" (nth 1 (nth (+ counter 0) tokenlist)))
                
                (if (not (checkidexist (nth 0 (nth (+ counter 0) tokenlist)) idlist))
                (progn
                    (princ "There is no such identifier: ")
                    (princ (nth 0 (nth (+ counter 0) tokenlist)))
                    (princ #\newline)
                    (return-from checkIfExp "false")
                )
                )

                (defparameter counter (+ counter 1))
                (return-from checkIfExp (get-value (nth 0 (nth (- counter 1) tokenlist)) idlist))
            )

            (   (string-equal "VALUEI" (nth 1 (nth (+ counter 0) tokenlist)))
                (defparameter counter (+ counter 1))
                (return-from checkIfExp (nth 0 (nth (- counter 1) tokenList)))
            )

            (   (string-equal "VALUEF" (nth 1 (nth (+ counter 0) tokenlist)))
                (setq pay (get-prefix (nth 0 (nth (+ counter 0) tokenList)) #\f))
                (setq payda (get-postfix (nth 0 (nth (+ counter 0) tokenList)) #\f))


                (defparameter counter (+ counter 1))
                (return-from checkIfExp  (/ pay payda))
            )
            
            ( (string-equal "OP_OP" (nth 1 (nth (+ counter 0) tokenlist)))
                (defparameter counter (+ counter 1))
    
                (cond 
                    ; aritmetik bir sey
                    ((checkOperatorParse (nth 1 (nth (+ counter 0) tokenlist)))

                        ;(setq operator (nth 1 (nth (+ counter 1) tokenlist)))
                        (let ((operator (nth 1 (nth (+ counter 0) tokenlist))))
                            
                            (defparameter counter (+ counter 1))
                            (let ((num1 (checkIfExp tokenlist)) (num2 (checkifexp tokenlist)))

                                (if (and (or (eq (type-of num1) 'bit) (eq (type-of num1) 'ratio) (equal (car (type-of num1)) 'integer)) 
                                            (or (eq (type-of num2) 'bit) (eq (type-of num2) 'ratio) (equal (car (type-of num2)) 'integer)))

                                    (cond 
                                        (   (and (or (equal (type-of num1) 'bit) (equal (type-of num1) 'ratio) (equal (car (type-of num1)) 'integer))  (or (equal (type-of num2) 'bit) (equal (type-of num2) 'ratio) (equal (car (type-of num2)) 'integer)))
                                            (if (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist)))
                                                (progn
                                                    (defparameter counter (+ counter 1))
                                                    (return-from checkIfExp (calculateArithmetic num1 num2 operator))
                                                )
                                                
                                            )
                                        )
                                    
                                    )

                                )
                            )
                            (return-from checkIfExp "false")
                        )
                    )

                    ; assignment
                    (  (checkIfAsg tokenlist)
                    ;    (princ "Syntax is OK") ; assigmennt ayarlanmadi
                       (return-from checkifexp 1)
                    )


                    (   (checkIfFcall tokenList)
                        (return-from checkifexp 1)
                    )

                    (   (string-equal "if" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))
                        (setq conditionResult (checkifexpb tokenlist))

                        (cond
                            (   (string-not-equal "false" (write-to-string conditionResult))
                                (setq exp1 (checkifexplist tokenlist))
                                (setq exp2 (checkifexplist tokenlist))
                                
                                (cond 
                                    (   (and (or (eq (type-of exp1) 'bit) (eq (type-of exp1) 'ratio) (equal (car (type-of exp1)) 'integer)) 
                                                (or (eq (type-of exp2) 'bit) (eq (type-of exp2) 'ratio) (equal (car (type-of exp2)) 'integer)) (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                                        (defparameter counter (+ counter 1))
                                        (if conditionResult (return-from checkIfExp exp1))
                                        (if (not conditionResult) (return-from checkIfExp exp2))

                                        (return-from checkIfExp exp2)
                                    )

                                    ( t
                                        (return-from checkIfExp "false")
                                    )
                                )
                            )
                        
                        )
                        (return-from checkIfExp "false")
                    )

                    (   (string-equal "while" (nth 0 (nth (+ counter 0) tokenlist)))
                        (defparameter counter (+ counter 1))
                        (setq conditionResult (checkifexpb tokenlist))

                        (cond
                            (   (string-not-equal "false" (write-to-string conditionResult))
                                (setq exp1 (checkifexplist tokenlist))
                                
                                (cond 
                                    (   (and (or (eq (type-of exp1) 'bit) (eq (type-of exp1) 'ratio) (equal (car (type-of exp1)) 'integer)) 
                                                (string-equal "OP_CP" (nth 1 (nth (+ counter 0) tokenlist))))
                                        (defparameter counter (+ counter 1))
                                        (if conditionResult (return-from checkIfExp exp1))

                                        (return-from checkIfExp exp1)
                                    )

                                    ( t
                                        (return-from checkIfExp "false")
                                    )
                                )
                            )
                        
                        )
                        (return-from checkIfExp "false")
                    )

                    (t
                    (return-from checkIfExp "false")
                    )
                )
            )
            
            ( t
                (return-from checkIfExp "false")
            )
    )
    
)

(load "lexer.lisp")
(setq outputlist '()) ; make the current list empty


          

(defun readfromtext (filestatus)

    (setq input (read-line filestatus nil))
    (setq inputList (coerce input 'list)) ; make the input a list
    (setq currentList '()) ; make the current list empty
 
    (setq idFlag 0)
    (setq numberFlag 0)
    (setq fnumberFlag 0)
    (setq strflag 0)
    (princ #\linefeed)
    
    (if (equal input nil)
        (return-from readfromtext )
    )
    (setq outputList (iterateInput inputList currentList idFlag numberFlag fnumberFlag strflag))

    ; (princ #\linefeed)
    ; (princ "The final list is: ")
    ; (princ outputlist)
    ; (princ #\linefeed)
    (return-from readfromtext (readfromtext filestatus))

)





(defun parseOutput ()
    

    (princ #\newline)
    (cond
        (   (equal (length outputlist) 0)
            (progn (return-from parseoutput))
        )
)

    (setq a(checkiffunc outputlist))


    (if (or (eq (type-of a) 'bit) (eq (type-of a) 'ratio) (equal (car (type-of a)) 'integer))
    (progn
        (princ "Syntax OK, ")
        (princ "Result is: ")
        (princ a)
        (princ #\newline)
        (setq outputlist (nthcdr counter outputlist))
        (defparameter counter 0)
        (parseOutput)
        (return-from parseOutput)
    ))

    (defparameter counter 0)
    (setq a(checkifexp outputlist))


    (if (or (eq (type-of a) 'bit) (eq (type-of a) 'ratio) (equal (car (type-of a)) 'integer))
    (progn
        (princ "Syntax OK, ")
        (princ "Result is: ")
        (princ a)
        (princ #\newline)
        (setq outputlist (nthcdr counter outputlist))
        (defparameter counter 0)
        (parseOutput)
        (return-from parseOutput)
    ))

    (defparameter counter 0)
    (setq a(checkifexplist outputlist))


    (if (or (eq (type-of a) 'bit) (eq (type-of a) 'ratio) (equal (car (type-of a)) 'integer))
    (progn
        (princ "Syntax OK, ")
        (princ "Result is: ")
        (princ a)
        (princ #\newline)
        (setq outputlist (nthcdr counter outputlist))
        (defparameter counter 0)
        (parseOutput)
        (return-from parseOutput)
    ))

    (if (string-equal  a "false")
    (progn
        (princ "Syntax error")
    ))



)

(defun gppinterpreter (filename)
    
    (setq filestatus nil)

    (cond
    (   (not (null filename))
        (setq filestatus (open filename))
        (princ "File is being readed...")
        (princ #\linefeed)
        (readfromtext filestatus)
        (parseOutput)

    )
    
    ( t
    
    (princ #\linefeed)
    (princ "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
    (princ #\linefeed)
    (princ "Please enter line to parse: ")
    (setq input (read-line)) ; Get the all input and assign to a variable
    (setq inputList (coerce input 'list)) ; make the input a list
    (setq currentList '()) ; make the current list empty
    (setq outputlist '()) ; make the current list empty
    (setq idFlag 0)
    (setq numberFlag 0)
    (setq fnumberFlag 0)
    (setq strflag 0)
    (princ #\linefeed)
    
    (setq outputList (iterateInput inputList currentList idFlag numberFlag fnumberFlag strflag))

    (princ #\linefeed)


    (setq a(checkiffunc outputlist))
    (defparameter counter 0)


    (if (or (eq (type-of a) 'bit) (eq (type-of a) 'ratio) (equal (car (type-of a)) 'integer))
    (progn
        (princ "Syntax OK, ")
        (princ "Result is: ")
        (princ a)
        (gppinterpreter filename)
        (return-from gppinterpreter)
    ))

    
    (setq a(checkifexp outputlist))
    (defparameter counter 0)


    (if (or (eq (type-of a) 'bit) (eq (type-of a) 'ratio) (equal (car (type-of a)) 'integer))
    (progn
        (princ "Syntax OK, ")
        (princ "Result is: ")
        (princ a)
        (gppinterpreter filename)
        (return-from gppinterpreter)
    ))

    (setq a(checkifexplist outputlist))
    (defparameter counter 0)

    (if (or (eq (type-of a) 'bit) (eq (type-of a) 'ratio) (equal (car (type-of a)) 'integer))
    (progn
        (princ "Syntax OK, ")
        (princ "Result is: ")
        (princ a)
        (gppinterpreter filename)
        (return-from gppinterpreter)
    ))

    (if (string-equal (write-to-string a) "false")
    (progn
        (princ "Syntax error")
    ))






    
    
    
    
    


    
    )
))

(setq filename (car *args*))
(gppinterpreter filename)

