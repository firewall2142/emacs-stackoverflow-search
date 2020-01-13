(require 'url)
(require 'json)

(defconst so-google-base-url "https://www.google.com/search?q=%s")
(defconst so-regex "stackoverflow.com/questions/\\([[:digit:]]+\\)")
(defconst so-api-base-url-question "https://api.stackexchange.com/2.2/questions/%s?site=stackoverflow")
(defconst so-api-base-url-question-answers "https://api.stackexchange.com/2.2/questions/%s/answers?site=stackoverflow&sort=votes")

(defun so-extract-question-numbers (google-search-result-buffer)
  "extract stackoverflow questions from given buffer containing webpage of google search results"
  (interactive)
  (with-current-buffer google-search-result-buffer
    (save-excursion
      (let ((ans '()))
	(while (re-search-forward so-regex nil t)
	  (let ((question-number))
	    (setq question-number (match-string-no-properties 1))
	    (if (null (member question-number ans))
		(setq ans (cons question-number ans)))
	    ;;(message "got question=%S; ans=%S" question-number ans)
	    ))
	ans))))

(defun so-search-google (query)
  "search on google and return the webpage as a buffer"
  (message "searching for %s on google" query)
  (let ((recieved-buffer
	(url-retrieve-synchronously (format so-google-base-url
					    (so-clean-string query)))
	))
    (message "recieved buffer %S" recieved-buffer)
    recieved-buffer))
	

(defun so-clean-string (string-to-clean)
  "converts string to be put in url ie: hello world this is sparta=> hello+world+this+is+sparta"
  (interactive "sString to clean: ")
  (replace-regexp-in-string " " "+" string-to-clean))

(defun so-vectorize (list-of-entities)
  (let ((ans ""))
    (dolist (x list-of-entities ans)
      (setq ans (format "%s%s;" ans x)))
    (substring ans 0 (1- (length ans)))))

(defun so-api-questions (list-of-question-ids)
  "call stackexchange api and return response in a buffer"
  (let (( response (url-retrieve-synchronously
		    (format so-api-base-url-question
			    (so-vectorize list-of-question-ids)))))
    (so-parse-json-buffer response)))


(defun so-google-search-question (query)
  "search on google for QUERY and return the question ids list"
  (interactive "sEnter query: ")
  (let ((clean-query (so-clean-string query))) ;;clean the query
    (message "cleaned query = %s" clean-query)
    (so-extract-question-numbers (so-search-google clean-query))))


(defun so-parse-json-buffer (json-buffer-or-name)
  (interactive "bEnter buffer containing json: ")
  (with-current-buffer json-buffer-or-name
    (save-restriction
      (goto-char (point-min))
      (re-search-forward "^$")
      (narrow-to-region (point) (point-max))
      (json-read))))

(defun so-vector-to-list (vec)
  (mapcar 'identity vec))

(defun so-json-get-items (parsed-json)
  "return items list from original parsed json"
  (so-vector-to-list (cdr (assoc 'items parsed-json))))

(defun so-loose-equality (num1 num2)
  (equal (format "%s" num1) (format "%s" num2)))

(defun so-get-question-title-from-item (item)
  (cdr (assoc 'title item)))

(defun so-get-question-id-from-item (item)
  (cdr (assoc 'question_id item)))

(defun so-json-get-question-item-from-parsed-json (parsed-json question-id)
  "get element in items list with question_id as given as argument otherwise return nil"
  (let ((items-list (so-json-get-items parsed-json)))
    (let ((question-item nil))
      (dolist (current-item items-list question-item)
	(if (so-loose-equality
	     (so-get-question-id-from-item current-item)
	     question-id)
	    (setq question-item current-item)
	  )))))


(defun so-get-question-title-id (parsed-json)
  (let ((items-list (so-json-get-items parsed-json))
	(id-title-alist '() ))
    (dolist (current-item items-list)
      (let ((current-id (so-get-question-id-from-item current-item))
	    (current-title (so-get-question-title-from-item current-item)))
	(setq id-title-alist (cons (cons current-title current-id) id-title-alist))))
    id-title-alist))

(defun so-api-question-answers (list-of-question-ids)
  "call stackexchange api and get json response in associativity list"
  (let (( response-buffer (url-retrieve-synchronously
		    (format so-api-base-url-question-answers
			    (so-vectorize list-of-question-ids)))))
    response-buffer))


(defun so-api-question-title-id (question-ids-list)
  (let* ((api-json-response
			     (so-api-questions question-ids-list)))
    (so-get-question-title-id api-json-response)))

;; (setq test-query "brainfuck stackoverflow")
;; (setq test-tid (so-api-question-title-id
;; 		(so-google-search-question test-query)))

(defun so-helm-question-source (question-title-id-list)
  (let ((qtids question-title-id-list))
    `((name . "Questions")
      (candidates . ,(mapcar 'car qtids))
      (action . (lambda (title)
		  (let ((qid (cdr
			      (assoc
			       title question-title-id-list))))
		    (message "selected id = %S" qid)))))
    ))

(defun so-helm-display-questions (question-title-id-list)
  (message "%s" question-title-id-list)
  (helm :sources (so-helm-question-source question-title-id-list)))

;; (so-helm-display-questions test-tid)










;; (defun so-get-answers (question-ids-list)
;;   (let* ((response-buffer (so-api-question-answers question-ids-list) )
;; 	 (parsed-json (so-parse-json-buffer response-buffer))
;; 	 (items-list (so-json-get-items parsed-json))
;; 	 (question-id-answers-list '())) ;; ((question_id answer1-body answer2 ...) ..)

;;     ;; create empty answer list for each question
;;     (dolist (question-id question-ids-list)
;;       (setq question-id-answers-list
;; 	    (cons (cons question-id '()) question-id-answers-list)))

;;     ;; iterate through

;; (defun so-json-search-question-id (parsed-json question-id)
;;   (dolist (ele (so-json-get-items parsed-json))

;; (print (so-google-search-question "stackoverflow hello world brain fuck"))
;; ("51579460" "33991173" "40566513" "21004459" "16836860") ;; brain fuck hello worlds
