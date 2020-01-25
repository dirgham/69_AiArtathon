;;; 2020-01-24- Dirgham
;;; Create blender objects

(ql:quickload :cl-json)
(ql:quickload :drakma)
(ql:quickload :aserve)
(ql:quickload :chirp)
(ql:quickload :cl-html-parse)
;; (ql:quickload :timer) this one didn't work
(ql:quickload :vecto) ;; vecto (saves vectors to png file)

;;; By Dirgham 23-11-2019
;;; recipes of lisp #21-1 pg 662
;;; this one saves complete lisp objects, mainly hashtble
(ql:quickload :cl-store)
;;; save the hashtable to the file:
 
;; temp disabled check function definition below
;;;(cl-store:store *ht-countries* "f:/test-store")
;;; thos one to restore it back to lisp
;;; (defparameter *other-thing*
;;;  (cl-store:restore "f:/test-store"))
;;; check function definition somewhere below:

(ql:quickload :CL-SMTP) ;; SMTP reference https://www.quicklisp.org/beta/UNOFFICIAL/docs/cl-smtp/readme.html
(ql:quickload :cl-ppcre) ;;regular expressions, reference: http://weitz.de/cl-ppcre/
(defpackage :dirgham
  (:use :cl :common-lisp-user :net.aserve :net.html.generator :cl-ppcre :sb-thread :sb-ext  :CL-SMTP #:vecto  ))  ;;  :chirp
;;;the following command to start the :dirgham package
(in-package :dirgham)
(setq chirp:*OAUTH-API-SECRET* "xxxx") ;to be replaced by personal credentials
(setq chirp:*OAUTH-ACCESS-SECRET* "xxx") ;to be replaced by personal credentials
(setq chirp:*OAUTH-ACCESS-TOKEN* "xxx") ;to be replaced by personal credentials
(setq chirp:*OAUTH-API-KEY* "xx") ;to be replaced by personal credentials

;; Python:
;       import bpy
;       bpy.context.area.ui_type = 'INFO'
;       bpy.ops.mesh.primitive_cube_add(size=2, enter_editmode=False, location=(0, 0, 0))
;       bpy.ops.mesh.primitive_uv_sphere_add(radius=1, enter_editmode=False, location=(0, 0, 0))
;  camera: bpy.ops.object.camera_add(enter_editmode=False, align='VIEW', location=(0, 0, 0), rotation=(1.10871, 0.0132652, 1.14827))

;; blender shell command blender --background --python artathon.py
 
(setq p (merge-pathnames "C:\\Users\\hp\\artathon.py"))
(setq bheader1 "import bpy" )
(setq bheader2 "bpy.context.area.ui_type = \'INFO\'" )
(setq bcamera1 "bpy.ops.object.camera_add(enter_editmode=False, align='VIEW', location=(7.35889, -6.92579, 4.95831), rotation=(63.5593, 0, 46.6919))")
(setq bcamera1active "bpy.context.scene.camera = bpy.data.objects[\"Camera\"]" )

;objectToSelect = bpy.data.objects["Camera"]
;objectToSelect.select_set(True)    
;bpy.context.view_layer.objects.active = objectToSelect

(setq aligncamera "bpy.ops.view3d.camera_to_view_selected()")
(with-open-file (s p :direction :output :if-exists :append)
  (format s "~a~%~a~%~a~%" bheader1  bcamera1 bcamera1active )) 

(defun bcube (size x y z)
(with-open-file (s p :direction :output :if-exists :append)
  (format s "bpy.ops.mesh.primitive_cube_add(size=~a, enter_editmode=False, location=(~a, ~a, ~a))~%"  size x y z  )   )
)

(defun bsphere (size x y z)
(with-open-file (s p :direction :output :if-exists :append)
  (format s "bpy.ops.mesh.primitive_uv_sphere_add(radius=~a, enter_editmode=False, location=(~a, ~a, ~a))~%"  size x y z  )   )
)

;;;; twitter parts
(defun clean-text (x)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) x))

(defun first-word (x)
 (subseq (clean-text x) 0 (position #\Space (clean-text x) :test #'equal)))

;; Actions so far: lisp, define, random,
(defun action (fw txt)

  (if (equal (string-downcase fw) "lisp") (eval (read-from-string (clean-text (string-trim  "lisp" (string-downcase txt) ))))
    (if (equal (string-downcase fw) "define") (print (clean-text (string-trim  "define" txt)))
      (if (equal (string-downcase fw) "random") ( print "rraannddoomm" )))))

; (action (first-word (clean-text *tweet*)))

;; this *xs* contains a list for excluded chars
(setq *xs* '(#\! #\@ #\# #\$ #\% #\^ #\& #\* #\_ #\- #\= #\+ #\< #\> #\, #\. #\? #\'
             #\:  #\; #\ARABIC_QUESTION_MARK #\/ #\| #\{ #\} #\[ #\] #\" #\~
             #\ARABIC_COMMA
             #\ARABIC_KASRATAN  #\ARABIC_KASRA #\ARABIC_FATHATAN
             #\ARABIC_FATHA   #\ARABIC_DAMMA  #\ARABIC_DAMMATAN
             #\ARABIC_SHADDA   #\ARABIC_SUKUN  #\ARABIC_TATWEEL
             ;; I put in substitute replace with space  #\newline  #\return #\tab
             ))


(defun simplify-txt (txt) ;; must use the *xs* list for exclude chars
  ;; متبقي ة و ه 
  (let (( mytxt txt))
    (dolist (i *xs* )
      ;; (format t "item: ~a~%" (remove i txt ))
      (setf mytxt (remove i mytxt))
      )
    (setq mytxt  (substitute  #\space #\tab  mytxt))
    (setq mytxt  (substitute  #\ARABIC_LETTER_ALEF  #\ARABIC_LETTER_ALEF_WITH_MADDA_ABOVE mytxt))
    (setq mytxt  (substitute  #\ARABIC_LETTER_ALEF  #\ARABIC_LETTER_ALEF_WITH_HAMZA_BELOW mytxt))
    
    (setq mytxt  (substitute  #\ARABIC_LETTER_ALEF #\ARABIC_LETTER_ALEF_WITH_HAMZA_ABOVE mytxt))
    (setq mytxt  (substitute  #\space #\newline   mytxt ))
    (setq mytxt  (substitute  #\space #\return mytxt ))
    mytxt
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; split the sentence string into words
(defun my-split-string (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

 ;;; removes the space between words
(defun clean-text (sent)
  (remove-if #'(lambda (m) (equal 0 (length m))) (my-split-string sent)))

(setq sample-img '( "shape1.jpeg"  "shape2.jpeg"  "shape3.jpeg"  "shape4.jpeg"  "shape5.jpeg" 
                    "shape6.jpeg"  "shape7.jpeg"  "shape8.jpeg"  "shape9.jpeg"  "shape10.jpeg" "shape11.jpeg" "shape12.jpeg"
                      "shape13.jpeg" "shape14.jpeg" "shape15.jpeg"  "shape16.jpeg" "shape17.jpeg" "shape18.jpeg" "shape19.jpeg" "shape20.jpeg"))

(defun select-random-image ()
 
 (nth (+ 1 (random 18))  sample-img)
 ;(merge-pathnames "C:\\Users\\hp\\shape1.jpg")
 
 )

;;; 
(defvar message_content)
(defvar message_sender)

(defun process-message (message)
  (progn
    ( print message ) ;;prints tweet status, ID, user
    (print  (chirp:screen-name (chirp:user message))) ;; prints screen name of sender
    (print  (chirp:xml-decode (chirp:text-with-expanded-urls message))) ;; prints human readable tweet text full

    (setq tw (chirp:xml-decode (chirp:text-with-expanded-urls message)))
    (DEFPARAMETER *TWEET* tw)
    (setf *tweet* (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) *tweet*))
    (setf tuser (chirp:screen-name (chirp:user message)))
    (setf aaa (string-trim " " (string-trim  "@taleenai" *tweet*)))

    (setf fw (string-downcase (first-word aaa)))
    (setf result (string-trim fw aa))
    ;; temp disabled  (setq message_response (eval (with-input-from-string (in aaa)  (read in))))
    ;; (setq message_response (eval (read-from-string aaa  )))

    (chirp:reply message :file (merge-pathnames (select-random-image)) )
                                        ;  (write-to-string tw))

                 )
    )
  

;;;;
(defun strange() 
(setq *run-response-thread* nil)
(defvar *th* nil)
(setf *th*
      (bt:make-thread
       (lambda ()
         (loop for i from 1 to 100 do
               (progn

                 (chirp:start-stream
                  :user #'(lambda (message)
                            (when (and (typep message 'chirp:status) (chirp:direct-mention-p message))
                              (process-message message )      )
                            T))

                 )
               when  *run-response-thread* do (return)


               ))

       :name "controlled thread - response2"

       )


      )
)

(defun process-message (message)
  (write-to-string  message)
  (chirp:reply message :file (merge-pathnames (select-random-image)) )   )


;;; put on hold
  (defun process-message (message )
  (format  #.*standard-output* "~&Message: ~a" message)
  (when (typep message 'chirp:status)
    (signal!  (new-tweet string string)
              (chirp:screen-name (chirp:user message))
              (chirp:xml-decode (chirp:text-with-expanded-urls message)))))


;;; put on hold
(defun process-message (message)
  (progn
    ( print message )
     (DEFPARAMETER *TWEET* message)
    (setf *tweet* (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) *tweet*))

    (setf aaa (string-trim " " (string-trim  "@taleenai" *tweet*)))
    (setf fw (string-downcase (first-word aaa)))

    (print fw)
    (print aaa)

    
     (chirp:reply message :file (merge-pathnames (select-random-image)) )   )
    )
  

;(setq *msgs*  (chirp:statuses/mentions-timeline ))

(setq *msgs* '() ) ;:snice-id   (write-to-string last-status-id) ))
(setq last-status-id  1220828029129252864)
(defun stream-get-mentions () ;; an attempt to collect que of mentions, tested on 14-09-2019 ok
  (setq *stream-get-mentions-thread*
        (bt:make-thread
         (lambda ()
          
             (progn
              
               (chirp:map-timeline  :mentions   #'(lambda (status)

                                        (progn 
                                           (format #.*standard-output* "test: ~a~%" status)

                                           (if (not (find status *msgs*)     )
                                              (progn 
                                                 (push status *msgs*)
                                                 (format #.*standard-output*  "from: ~a ~% ~a~%"  (chirp:xml-decode (chirp:text-with-expanded-urls status)) (chirp:user status)  )
                                                  ;(chirp:reply status "Hi" :file (merge-pathnames "C:\\Users\\hp\\star.png") )
                                                (chirp:reply status "Hi" :file (merge-pathnames (select-random-image)) ) 
                                                 (setq last-status-id  (chirp:id (nth 0 *msgs*)))
                                                 (sleep 5)
                                             ) 
                                           )
                                           )
                                         
                                             ; )
                                           ;;          
                                           ;;       )
                                           ;; decided to take responses on another thread
                                               )
                                                    :since-id   (write-to-string  last-status-id  )   :count 1        )
           
                ;;; other functions add below
              

               )
             ))))







;;;(action (first-word (clean-text aa)))

;;; Cl-ZPNG
(defun draw-rgb (file)
  (let ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width 200
                             :height 200)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (loop for a from 38 to 255 by 31
	do (loop for b from 10 to 255 by 10
	     do (loop for g from 38 to 255 by 31
		  do (loop for r from 10 to 255 by 10
			do (write-pixel (list r g b a) png)))))
      (finish-png png))))

