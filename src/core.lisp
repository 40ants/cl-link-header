(defpackage #:link-header/core
   (:nicknames #:link-header)
   (:use #:cl
         #:f-underscore)
   (:import-from #:alexandria
                 #:make-keyword)
   (:import-from #:cl-strings
                 #:ends-with)
   (:export
    #:with-links
    #:parse))
(in-package link-header/core)


(defun parse (text)
  "Parses content of the link HTTP header, as specified at https://www.w3.org/wiki/LinkHeader.

Multiple links can be separated by commas.

Returns plist where \"rel\" is a key."

  (let (result)
    (cl-ppcre:do-register-groups
        (url relation)
        ("<([^>]*)>; rel=\"([^\"]*)\"" text)

      (push url result)
      (push (make-keyword (string-upcase relation))
            result))
    
    result))


(defmacro with-links ((&rest args) form &rest body)
  "Evaluates a form which should be a call to dexador:get and evaluates body in the context
   where variables response, status-code, headers and links are bound.

   Here response, status-code and headers are the same values which a call to dexador returns,
   and links is a plist containing a links from the Link HTTP header, if it is avalable."
  (alexandria:with-gensyms (response-var
                            status-code-var
                            headers-var
                            uri-var
                            stream-var
                            links-var)
    (let* ((supported-var-names (list 'response
                                      'status-code
                                      'headers
                                      'uri
                                      'links))
           ;; This var will be not nil if user wants to
           ;; extract one or more links from the Link header
           (links-requested-p
             (remove-if-not (f_ (cl-strings:ends-with (symbol-name _)
                                                      "-LINK"))
                            args))
           (bind-clauses
             (append
              (when links-requested-p
                `((,links-var (parse (gethash "link" ,headers-var "")))))
              (loop for arg in args
                    for arg-name = (string-downcase (symbol-name arg))
                    for clause = (cond
                                   ((string= arg-name
                                             "response")
                                    (list arg response-var))
                                   ((string= arg-name
                                             "status-code")
                                    (list arg status-code-var))
                                   ((string= arg-name
                                             "headers")
                                    (list arg headers-var))
                                   ((string= arg-name
                                             "uri")
                                    (list arg uri-var))
                                   ((string= arg-name
                                             "links")
                                    (list arg links-var))
                                   ((ends-with arg-name "-link")
                                    (list arg
                                          `(getf ,links-var
                                                 ;; Here we are extracting a link with
                                                 ;; a name made from given argument name
                                                 ;; but without a '-link' suffix
                                                 ,(make-keyword
                                                   (string-upcase
                                                    (subseq arg-name
                                                            0
                                                            (- (length arg-name)
                                                               5)))))))
                                   (t (error "Name ~A is not supported. Only ~{~a~#[~; and ~:;, ~]~} are supported. Or add suffix '-link' if you want to refer to a link from the Link header, for example, use NEXT-LINK."
                                             arg
                                             supported-var-names)))
                    when clause
                      collect clause))))
      `(multiple-value-bind (,response-var
                             ,status-code-var
                             ,headers-var
                             ,uri-var
                             ,stream-var)
           ,form
         (declare (ignorable ,response-var
                             ,status-code-var
                             ,headers-var
                             ,uri-var
                             ,stream-var))
         (let* (,@bind-clauses
                ;; (next-link (getf ,links-var :next))
                )
           ;; (declare (ignorable ,links-var))
           ,@body)))))
