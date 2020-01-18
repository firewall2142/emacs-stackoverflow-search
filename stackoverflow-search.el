(require 'json)
(require 'url)
(require 'cl-lib)

(defvar *so-json-response* nil
  "variable containing the most recent api call's json response")
(defconst so-google-base-url "https://www.google.com/search?q=site:stackoverflow.com %s&num=50")
(defconst so-regex "stackoverflow.com/questions/\\([[:digit:]]+\\)")
(defconst so-api-base-url-question
  "https://api.stackexchange.com/2.2/questions/%s?order=desc&sort=activity&site=stackoverflow&filter=!.Iwe-BLQGuP55LzGq_b4LffyXSIpy")


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
   return: ((title.id) ...) where title is of the form [votes] <actual-question-title>"
  (let ((items-list (so-json-get-items *so-json-response*))
	(id-title-alist '() ))
    (dolist (current-item items-list)
      (let ((current-id (so-get-question-id-from-item current-item))
	    (current-title (format "%s" (so-get-question-title-from-item current-item))))
	(setq id-title-alist (cons (cons (replace-html current-title) current-id) id-title-alist))))
    id-title-alist))


(defun so-loose-equality (num1 num2)
  (equal (format "%s" num1) (format "%s" num2)))

(defun so-get-question-item-from-id (items-list question-id)
  (let ((item-found nil))
    (dolist (item items-list item-found)
      (if (so-loose-equality (so-get-question-id-from-item item) question-id)
	  (setq item-found item)))))


(defun get-question-answers (question-id)
  "params: question-id
   returns: (answer1 answer2 ...)"
  (let* ((items-list (so-json-get-items *so-json-response*))
	 (question-item (so-get-question-item-from-id items-list question-id))
	 (answer-items-list (so-vector-to-list(cdr (assoc 'answers question-item)))))
    answer-items-list))

;;; HELM UI
(defun display-questions-ui ()
  "param:nothing
   does: makes helm-ui and attaches appropriate action"
  (let* ((question-title-ids (get-question-title-id-list))
	 (so-helm-source `((name ."Questions")
			   (candidates . ,question-title-ids)
			   (action . (lambda (ques-id)
				       ques-id)))))
    (helm :sources '(so-helm-source))))

(defun display-answer-ui (question-id)
  "param: answer-full-body
   does: display the answer in a buffer"
  (let* ((answers-alist (get-question-answers question-id))
	 (answer-body-list (displayable-answers answers-alist))
	 (zipped-body-alist (cl-mapcar
			     'cons
			     answer-body-list
			     answers-alist))
	 (so-helm-source `((name . "Answers")
			   (candidates . ,zipped-body-alist)
			   (action . (lambda (answer-alist)
				       answer-alist)))))
    (helm :sources '(so-helm-source))))

(defun replace-html (str)
  (setq str (replace-regexp-in-string "<[^<>]*>" " " str))
  (setq str (replace-regexp-in-string "[\t\n\\(  \\)]+" " " str))
  (setq str (replace-regexp-in-string "&amp;" "&" str))
  (setq str (replace-regexp-in-string "&lt;" "<" str))
  (setq str (replace-regexp-in-string "&gt;" ">" str))
  (setq str (replace-regexp-in-string "&#39;" "'" str))
  (setq str (replace-regexp-in-string "&quote;" "'" str))
  str)

(defun displayable-answers (answers-alist)
  (mapcar
   (lambda (ans-alist)
     (let ((ans (cdr (assoc 'body ans-alist))))
       (setq ans (replace-html ans))
     ans)
   )
   answers-alist))


(defun render-answer (answer-alist)
  "Render answer in a new buffer name *stackoverflow-answer*"
  (let ((answer-buffer "*stackoverflow-answer*"))
    (if (not (null (get-buffer answer-buffer)))
	(kill-buffer answer-buffer))
    (get-buffer-create answer-buffer)
    (let ((split-height-threshold nil)
	  (split-width-threshold 0))
      (display-buffer answer-buffer))
    (switch-to-buffer-other-window answer-buffer)
    (insert (cdr (assoc 'body answer-alist)))
    (shr-render-region (point-min) (point-max))
    (goto-char (point-min)))
  (special-mode))


(defun stackoverflow-search (query)
  (interactive "sEnter query: ")
  (let ((ques-ids (so-google-search-question query)))
    (so-stackexchange-api-call ques-ids)
    (let* ((sel-qid (display-questions-ui))
	   (sel-ans (display-answer-ui sel-qid)))
      (render-answer sel-ans))))

(stackoverflow-search "how to delete git branch")
