#lang racket
;;; We'll use Racket's net/url package to obtain our text,
;;; data-science to process the text, and plot to visualize the
;;; results 
(require net/url)
(require data-science-master)
(require plot) 
(require math)
(require json)

(require net/uri-codec
  web-server/stuffers/hmac-sha1
  net/base64
  rackunit)


(define oauth-single-user%
  (class object%
    (super-new)
    
    ;; mandatory fields, must be
    ;; set by user
    (init-field consumer-key)
    (init-field consumer-secret)
    (init-field access-token)
    (init-field access-token-secret)
    
    
    ;;;;oauth-single-user constants 
    (define time-stamp (number->string (current-seconds)))
    (define oauth-version "1.0")
    (define signature-method "HMAC-SHA1")
    (define nonce (string-append (number->string (current-seconds))
                 (number->string (random
                        (current-seconds) (make-pseudo-random-generator)))))
    
    (define (get-key-as-string key_and_value)
      (symbol->string (car key_and_value)))
    
    
    ;;; url helper functions
    (define (get-base-url request-url)
      (let ([old-url (string->url request-url)])
        (url->string (make-url (url-scheme old-url) #f 
                         (url-host old-url) #f #t 
                         (url-path old-url) empty #f))))
    
    (define (add-params-to-url base-url params)
      (let ([old-url (string->url base-url)])
        (url->string (make-url (url-scheme old-url) #f
                         (url-host old-url) #f #t
                         (url-path old-url) 
                         params
                         #f))))
                         
      
    ;creates string to be used in signature 
    (define (create-param-string list_of_keys param_string) 
      (cond 
        [(empty? list_of_keys) param_string]
        [(equal? param_string "")
         (create-param-string (rest list_of_keys)
                  (string-append (symbol->string(car (first list_of_keys)))
                        "="
                       (cdr (first list_of_keys))))]
        [else 
         (create-param-string (rest list_of_keys) 
                            (string-append param_string "&" 
             (symbol->string (car (first list_of_keys)))
          "="
          (cdr (first list_of_keys))))]))  
  
    ;create list of oauth params
    (define/private (create-oauth-keys-and-values) 
      (list (cons 'oauth_consumer_key consumer-key)
            (cons 'oauth_signature_method signature-method)
            (cons 'oauth_version oauth-version)
            (cons 'oauth_timestamp time-stamp)
            (cons 'oauth_nonce nonce)
            (cons 'oauth_token access-token)))
    
 (define (generate-base-string http-method base-url params)
  (string-append (string-upcase http-method) "&" 
   (uri-unreserved-encode base-url) "&" 
   (uri-unreserved-encode 
    (create-param-string 
     (sort (percent-encode-keys-and-values 
            (append (create-oauth-keys-and-values) params)) 
       string<? #:key get-key-as-string)
   ""))))  
    
 (define/private (generate-signing-key consumer-secret token-secret)
  ;;signature_key = consumer_secret || "&" || token_secret
  (string-append (uri-unreserved-encode consumer-secret)
   "&"
   (uri-unreserved-encode token-secret)))
    
  (define/private (generate-signature http-method base-url params)
    ;signature = base64(hmac-sha1(signature_base_string, signature_key))
    (uri-unreserved-encode 
    (bytes->string/utf-8
     ;base64-encode returns #"xxxx\r\n"
     ;use regexp-replace to take out \r and \n
     (regexp-replace #rx#"[\r\n]+$" (base64-encode (HMAC-SHA1
      ;create signing key
      (string->bytes/utf-8 
       (generate-signing-key consumer-secret access-token-secret))
      ;create signature base string 
      (string->bytes/utf-8 
       (generate-base-string http-method  base-url
                             params))))""))) )
    
    (define/private (generate-auth-header http-method base-url params)
      (string-append "Authorization: OAuth " 
                 "oauth_consumer_key=\"" consumer-key "\","
                 "oauth_nonce=\"" nonce "\"," 
                 "oauth_signature=\"" (generate-signature http-method base-url params) "\","
                 "oauth_signature_method=\"" signature-method "\","
                 "oauth_timestamp=\"" time-stamp "\","
                 "oauth_token=\"" access-token "\","
                 "oauth_version=\"" oauth-version "\""))
    
     ;percent encode all keys and values of parameter list
     (define (percent-encode-keys-and-values list-of-params)
       (map (lambda (param)
              (cons 
                (string->symbol(uri-unreserved-encode 
                               (symbol->string (car param))))
                (uri-unreserved-encode (cdr param))))
              list-of-params))
    
      (define/public (get-request base-url [params empty])
         (regexp-match
            #px".*"
            (get-pure-port
             (if (empty? params)
                 (string->url base-url)
                 (string->url (add-params-to-url base-url params)))
             (list
              (generate-auth-header "get" base-url params)))))
  
      (define/public (post-request base-url post-data)
        (regexp-match
            #px".*"
            (post-pure-port
             (if (empty? post-data)
                 (string->url base-url)
                 (string->url (add-params-to-url base-url post-data))) 
             ;post data
             (string->bytes/utf-8 (create-param-string 
                                   (sort 
                                    (percent-encode-keys-and-values post-data) 
                                    string<? #:key get-key-as-string) ""))
             (list
              (generate-auth-header "post" base-url post-data)))))))


(define twitter-oauth (new oauth-single-user%  
     [consumer-key "FNdM5Cve6zDULYGeENafpncfB"]
     [consumer-secret "Q0qezXAGOq130uLSQvq7g84CwP6yK4vCIoI1hRyUaFIwqNYlbe"]
     [access-token "808671618-xagI8SsSrarkJdbNXciXPIUAjA2nSjieI7lJOkJv"]
     [access-token-secret "pohTt67sUHb04Ki4q4LfeLbDP9y30mjHrABwEFPdF27Cw"]))

(define raw_tweets (car
  (send twitter-oauth get-request
        "https://api.twitter.com/1.1/search/tweets.json"
        (list (cons 'q "")
              (cons 'count "5")
              (cons 'geocode "1.3707295,32.3032414,200km") ;;; coordinates for uganda 
              (cons 'since "2018-11-06")
              (cons 'until "2018-11-07")))))

(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

(define (preprocess-text lst)
  (map (λ (x)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))
       lst))

(define tweets (string->jsexpr
                (with-input-from-bytes raw_tweets (λ () (json-lines->json-array)))))

(define t (car (car
                  (let ([tmp (map (λ (x) (list (hash-ref x 'statuses))) tweets)])
                    tmp))))

  ;;; Remove just the tweet text from each tweet hash and also filter out retweets.
  (define tweet-text
    (let ([tmp (map (λ (x) (list (hash-ref x 'text))) t)])
      (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))


;;; tweet-list->string abstraction enables us to extract tweets from a nested list
;;; to a string "pure-tweet-text". This is now ready for initial text preprocessing 
(define (tweet-list->string lst)
  (define y "")
    (for-each (lambda(arg)
                (set! y (string-append y " " arg)))
              (flatten lst)
    ) y)
(define pure-tweet-text (tweet-list->string tweet-text))

;;; ---------------------------------------------------------------
;;; Using abstractions from the data-science-master package
;;; ---------------------------------------------------------------

;;; Next, we capture the text from the above symbol (pure-tweet-text), removing capitalization, 
;;; punctuation, and then extra spaces
(define pure-tweet-text-processed
  (string-normalize-spaces
   (remove-punctuation
    (string-downcase pure-tweet-text) #:websafe? #t)))
			 

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens pure-tweet-text-processed #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 3)
;;; --> '(("word" "sentiment" "freq")
;;;       ("ship" "anticipation" 367)
;;;       ("sea" "positive" 364)
;;;       ("time" "anticipation" 318)
;;;       ("long" "anticipation" 311))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;;; --> '(("anticipation" 4739)
;;;       ("positive" 9206)
;;;       ("joy" 3196)
;;;       ("trust" 5095)
;;;       ("surprise" 2157)
;;;       ("negative" 7090)
;;;       ("fear" 4136)
;;;       ("sadness" 3317)
;;;       ("anger" 2765)
;;;       ("disgust" 1958))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))

