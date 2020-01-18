(require 'json)
(require 'url)

(defvar *so-json-response* nil
  "variable containing the most recent api call's json response")
(defconst so-google-base-url "https://www.google.com/search?q=%s")
(defconst so-regex "stackoverflow.com/questions/\\([[:digit:]]+\\)")
(defconst so-api-base-url-question
  "https://api.stackexchange.com/2.2/questions/%s?order=desc&sort=activity&site=stackoverflow&filter=!.Iwe-B)-NpGS._8.rRsprhjVhVXRm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; so-google-search-question
(defun so-clean-string (string-to-clean)
  "converts string to be put in url ie: hello world this is sparta=> hello+world+this+is+sparta"
  (interactive "sString to clean: ")
  (replace-regexp-in-string " " "+" string-to-clean))

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

(defun so-google-search-question (query)
  "search on google for QUERY and return the question ids list"
  (interactive "sEnter query: ")
  (let ((clean-query (so-clean-string query))) ;;clean the query
    (message "cleaned query = %s" clean-query)
    (so-extract-question-numbers (so-search-google clean-query))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test
;; (message "%s" (so-google-search-question "emacs change between windows quickly stackoverflow"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stackexchange-api-call
(defun so-vectorize (list-of-entities)
  (let ((ans ""))
    (dolist (x list-of-entities ans)
      (setq ans (format "%s%s;" ans x)))
    (substring ans 0 (1- (length ans)))))


(defun so-parse-json-buffer (json-buffer-or-name)
  (interactive "bEnter buffer containing json: ")
  (with-current-buffer json-buffer-or-name
    (save-restriction
      (goto-char (point-min))
      (re-search-forward "^$")
      (narrow-to-region (point) (point-max))
      (json-read))))

(defun so-stackexchange-api-call (list-of-question-ids)
  "params: questions-list
   side-effect: sets global variable *so-json-response* to parsed json response of the api call
   returns: *so-json-response*"
  (let (( response-buffer (url-retrieve-synchronously
		    (format so-api-base-url-question
			    (so-vectorize list-of-question-ids)))))
    (setq *so-json-response* (so-parse-json-buffer response-buffer))))

;; (so-stackexchange-api-call '(7394289 1774832 91071 10774995 4671819))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun so-vector-to-list (vec)
  (mapcar 'identity vec))

(defun so-json-get-items (parsed-json)
  "return items list from original parsed json"
  (so-vector-to-list (cdr (assoc 'items parsed-json))))

(defun so-get-question-id-from-item (item)
  (cdr (assoc 'question_id item)))

(defun so-get-question-title-from-item (item)
  (cdr (assoc 'title item)))

(defun get-question-title-id-list ()
  "no params, uses global *so-json-response* variable to get question titles and ids
   return: ((title.id) ...)"
  (let ((items-list (so-json-get-items *so-json-response*))
	(id-title-alist '() ))
    (dolist (current-item items-list)
      (let ((current-id (so-get-question-id-from-item current-item))
	    (current-title (so-get-question-title-from-item current-item)))
	(setq id-title-alist (cons (cons current-title current-id) id-title-alist))))
    id-title-alist))

;;(message "%s" (get-question-title-id-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun so-loose-equality (num1 num2)
  (equal (format "%s" num1) (format "%s" num2)))

(defun so-get-question-item-from-id (items-list question-id)
  (let ((item-found nil))
    (dolist (item items-list item-found)
      (if (so-loose-equality (so-get-question-id-from-item item) question-id)
	  (setq item-found item)))))

;; test (pp (so-get-question-item-from-id (so-json-get-items *so-json-response*) "7394289"))

(defun get-question-answers (question-id)
  "params: question-id
   returns: (answer1 answer2 ...)"
  (let* ((items-list (so-json-get-items *so-json-response*))
	 (question-item (so-get-question-item-from-id items-list question-id))
	 (answer-items-list (so-vector-to-list(cdr (assoc 'answers question-item))))
	 (answer-body-list '()))
    (dolist (answer-item answer-items-list answer-body-list)
      (setq answer-body-list (cons (cdr (assoc 'body answer-item)) answer-body-list)))))

;;test (message "%S" (get-question-answers 91071))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


