(require 'org-expenses)
(require 'ert)
(require 'f)
(require 'dash)

;;;; utilities
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     (let ((result (progn ,@body)))
       (message (format "%.06f\n" (float-time (time-since time))))
       result)))

(defmacro org-expenses/with-dir (dir &rest body)
  `(let ((dirslash (f-slash (expand-file-name ,dir)))
         (wd  default-directory))
     (f-mkdir dirslash)
     (setq default-directory dirslash)
     (unwind-protect
         (progn
           ,@body)
       (f-delete dirslash t)
       (setq default-directory wd))))
(put 'org-expenses/with-dir 'lisp-indent-function 1)

(defun org-expenses/f-delete (file)
  (when (f-exists? file)
    (f-delete file)))

(defmacro org-expenses/with-db (file &rest body)
  `(let ((old org-expenses/sqlite-db-file))
     (setq org-expenses/sqlite-db-file ,file)
     (unwind-protect
         (progn
           ,@body)
       (org-expenses/f-delete ,file)
       (setq org-expenses/sqlite-db-file old))))
(put 'org-expenses/with-db 'lisp-indent-function 1)

(defun org-expenses/doublef (x) (* x 2))
(defun org-expenses/square  (x) (* x x))

(defun org-expenses/parse-test-file ()
  (org-expenses/parse--file "test-exp1.org"))

(defun org-expenses/parse-string (str)
  (with-temp-buffer
    (insert str)
    (org-mode)
    (org-expenses/parse--file (current-buffer))))

(defun org-expenses/get-headline (name)
  "Parse 'test-exp1.org' and return first headline with NAME."
  (let ((ast (org-expenses/parse-test-file)))
    (car (org-element-map ast 'headline
           (lambda (el)
             (when (string= (org-element-property :raw-value el)
                            name)
               el))))))

(defun org-expenses/get-item (name)
  (let ((hl (org-expenses/get-headline name)))
    (org-expenses/hl--to-item hl "test-exp1.org")))

(defun org-expenses/plist-equal (p1 p2)
  (equal (-sum
          (org-expenses/plist-map p1
                             (lambda (k v1)
                               (let ((v2 (plist-get p2 k)))
                                 (if (equal v1 v2) 2 0)))))
         (length p1)))


;;;; tests

(ert-deftest org-expenses/test-file-list ()
  "Tests the expansion of expense files configured with `org-expenses/files'."
  (org-expenses/with-dir "testrun1"
    (f-touch "onefile.org")
    (setq org-expenses/files "onefile.org")
    (should (equal (org-expenses/file-list t) nil)))
  (org-expenses/with-dir "testrun2"
    (f-touch "onefile.org")
    (setq org-expenses/files (list "onefile.org"))
    (should (equal (org-expenses/file-list t) (list (abbreviate-file-name "onefile.org")))))
  (org-expenses/with-dir "testrun"
   (f-touch "file1.org")
   (f-touch "file2.txt")
   (f-touch "file3.org")
   (setq org-expenses/files (expand-file-name "."))
   (should (equal (org-expenses/file-list t)
                  (list (abbreviate-file-name (expand-file-name "file1.org"))
                        (abbreviate-file-name (expand-file-name "file3.org"))))))
  (setq org-exp-files nil))


(ert-deftest org-expenses/plist-map-test ()
  "Tests the plist-map function."
  (should (equal (org-expenses/plist-map '(:a 1 :b 2 :c 3)
                                    'cons)
                 '((:c . 3) (:b . 2) (:a . 1))))
  (should (equal (org-expenses/plist-map '(2 4 4 8 8 16)
                                    '+)
                 '(24 12 6)))
  (should (equal (org-expenses/plist-map '(:a 1 :b 2)
                                    'list t)
                 '(:b 2 :a 1))))

(ert-deftest org-expenses/plist-kmap-test ()
  "Tests the plist-kmap function."
  (should (equal (org-expenses/plist-kmap '(:a 1 :b 2 :c 3)
                                     'symbol-name)
                 '(":c" ":b" ":a")))
  (should (equal (org-expenses/plist-kmap '(:a 1 :b 2)
                                     'stringp)
                 nil)))

(ert-deftest org-expenses/string-number-p-test ()
  (should (equal (org-expenses/string-number-p "232") t))
  (should (equal (org-expenses/string-number-p "23.22") t))
  (should (equal (org-expenses/string-number-p ".2322") t))
  (should (equal (org-expenses/string-number-p "23.") t))
  (should (equal (org-expenses/string-number-p "a23.22") nil))
  (should (equal (org-expenses/string-number-p "23ab") nil))
  (should (equal (org-expenses/string-number-p :abc) nil))
  (should (equal (org-expenses/string-number-p "23.2.2") nil)))

(ert-deftest org-expenses/n+-test ()
  "Test for the n+ function."
  (should (equal (org-expenses/n+ "1" 2 "3") 6))
  (should (equal (org-expenses/n+ 1 nil :a 4) 5))
  (should (equal (org-expenses/n+ "22a" "9" 2) 11)))

(ert-deftest org-expenses/get-currency-test ()
  (should (equal (org-expenses/get-currency "usd") :USD))
  (should (equal (org-expenses/get-currency :eur) :EUR))
  (should (equal (org-expenses/get-currency :EUR) :EUR))
  (should (equal (org-expenses/get-currency "Usd") :USD))
  (should (equal (org-expenses/get-currency :none) nil)))

(ert-deftest org-expenses/format-currency ()
  (should (equal (org-expenses/format-currency :EUR) "EUR")))

(ert-deftest org-expenses/format-price-test ()
  (should (equal (org-expenses/format-price "23.2909090") "23.29"))
  (should (equal (org-expenses/format-price 23.2909090) "23.29"))
  (should (equal (org-expenses/format-price "23.2999") "23.30"))
  (should (equal (org-expenses/format-price "") ""))
  (should (equal (org-expenses/format-price 0) ""))
  (should (equal (org-expenses/format-price "abc") ""))
  (should (equal (org-expenses/format-price :abc) "")))

(ert-deftest org-expenses/parse-date-parts-test ()
  (should (equal (org-expenses/parse-date-parts "2014") '(2014)))
  (should (equal (org-expenses/parse-date-parts "2014-01") '(2014 1)))
  (should (equal (org-expenses/parse-date-parts "20a14-0z1") '(2014 1)))
  (should (equal (org-expenses/parse-date-parts "2014-01-02") '(2014 1 2))))

(ert-deftest org-expenses/expand-date-test ()
  (should (equal (org-expenses/expand-date "2014-01" :start) "[2014-01-01]"))
  (should (equal (org-expenses/expand-date "2014-01" :end) "[2014-01-31]"))
  (should (equal (org-expenses/expand-date "2014-02" :start) "[2014-02-01]"))
  (should (equal (org-expenses/expand-date "2014-02" :end) "[2014-02-28]")))

(ert-deftest org-expenses/abbrev-date-test ()
  (should (equal (org-expenses/abbrev-date "2014") "2014"))
  (should (equal (org-expenses/abbrev-date "2014-01") "2014"))
  (should (equal (org-expenses/abbrev-date "2014-01-01") "2014"))
  (should (equal (org-expenses/abbrev-date "2014-02") "2014-02"))
  (should (equal (org-expenses/abbrev-date "2014-02-10") "2014-02-10"))
  (should (equal (org-expenses/abbrev-date "2014-01-31") "2014"))
  (should (equal (org-expenses/abbrev-date "2014-12-31") "2014"))
  (should (equal (org-expenses/abbrev-date "2014-12") "2014"))
  (should (equal (org-expenses/abbrev-date "2014-07-31") "2014-07"))
  (should (equal (org-expenses/abbrev-date "2014-07-01") "2014-07")))

(ert-deftest org-expenses/expand-date-range-test ()
  (should (equal (org-expenses/expand-date-range "2014-02")
                 "[2014-02-01]--[2014-02-28]")))

(ert-deftest org-expenses/abbrev-date-range ()
  (should (equal (org-expenses/abbrev-date-range "2014") "2014"))
  (should (equal (org-expenses/abbrev-date-range "2014--2016") "2014--2016"))
  (should (equal (org-expenses/abbrev-date-range "[2014-02-01]--[2014-02-28]")
                 "2014-02"))
  (should (equal (org-expenses/abbrev-date-range "[2014-01-01]--[2014-12-31]")
                 "2014"))
  (should (equal (org-expenses/abbrev-date-range "[2014-02-11]--[2014-02-26]")
                 "[2014-02-11]--[2014-02-26]"))
  (should (equal (org-expenses/abbrev-date-range "[2014-02-15]--[2014-12-31]")
                 "[2014-02-15]--2014")))

(ert-deftest org-expenses/date-in-range-p-test ()
  (should (equal (org-expenses/date-in-range-p
                  (org-expenses/expand-date-range "2014-04") "[2014-02-03]")
                 nil))
  (should (equal (org-expenses/date-in-range-p
                  (org-expenses/expand-date-range "2014-04") "[2014-04-03]")
                 t)))

(ert-deftest org-expenses/date-range-tuple-test ()
  (should (equal (org-expenses/date-range-tuple "2014-04")
                 '("[2014-04-01]" . "[2014-04-30]")))
  (should (equal (org-expenses/date-range-tuple "2013")
                 '("[2013-01-01]" . "[2013-12-31]"))))

(ert-deftest org-expenses/date-range-p-test ()
  (should (equal (org-expenses/date-range-p "abc") nil))
  (should (equal (org-expenses/date-range-p "2014-2015") nil))
  (should (equal (org-expenses/date-range-p "2015--2014") nil))
  (should (equal (org-expenses/date-range-p "2014-10-10--2014-09-10") nil))
  (should (equal (org-expenses/date-range-p "2014") t))
  (should (equal (org-expenses/date-range-p "2014-02--2014-04") t))
  (should (equal (org-expenses/date-range-p "2014--2015") t))
  (should (equal (org-expenses/date-range-p "2013-10-10--2013-12-12") t))
  (should (equal (org-expenses/date-range-p "[2013-12-12]--[2013-12-15]") t)))

(ert-deftest org-expenses/date--range-simple-add-test ()
  (should (equal (org-expenses/date--range-simple-add "2014-08" 2)
                 "2014-10"))
  (should (equal (org-expenses/date--range-simple-add "2014-08-10" 2)
                 "[2014-08-12]"))
  (should (equal (org-expenses/date--range-simple-add "2014" 2)
                 "2016"))
  (should (equal (org-expenses/date--range-simple-add "2014-08-28" 7)
                 "[2014-09-04]"))
  (should (equal (org-expenses/date--range-simple-add "2014-04-28" 7)
                 "[2014-05-05]")))

(ert-deftest org-expenses/date-range-move-test ()
  (should (equal (org-expenses/date-range-move "2014-04" 2)
                 "2014-06"))
  (should (equal (org-expenses/date-range-move "2014-04-10--2014-04-20" 5)
                 "[2014-04-15]--[2014-04-25]"))
  (should (equal (org-expenses/date-range-move "2014-06-05" -8)
                 "[2014-05-28]"))
  (should (equal (org-expenses/date-range-move "2014-04--2014-05" -24)
                 "2012-04--2012-05")))

(ert-deftest org-expenses/date-range-extend-test ()
  (should (equal (org-expenses/date-range-extend "2014" 2)
                 "2014--2016"))
  (should (equal (org-expenses/date-range-extend "2014-04" 2)
                 "2014-04--2014-06"))
  (should (equal (org-expenses/date-range-extend "2014-04--2014" -1)
                 "2014-04--2014-11"))
  (should (equal (org-expenses/date-range-extend "2014" -4)
                 "2014--2014-08"))
  (should (equal (org-expenses/date-range-extend "2014-04" -2)
                 "2014-04--[2014-04-28]"))
  (should (equal (org-expenses/date-range-extend "2014-04--2014-06" -5)
                 "2014-04--[2014-06-25]")))

(ert-deftest org-expenses/make--filter-test ()
  (let ((f1 (org-expenses/make--filter '(numberp integerp))))
    (should (equal (funcall f1 2.2) nil))
    (should (equal (funcall f1 4) t)))
  (let ((f2 (org-expenses/make--filter '(:or numberp integerp))))
    (should (equal (funcall f2 2.2) t))
    (should (equal (funcall f2 5) t)))
  (let ((f3 (org-expenses/make--filter '(:not numberp integerp))))
    (should (equal (funcall f3 "u") t))
    (should (equal (funcall f3 2.2) nil))
    (should (equal (funcall f3 5) nil)))
  (let ((f4 (org-expenses/make--filter nil)))
    (should (equal (funcall f4 "z") "z"))))

(ert-deftest org-expenses/hl--has-property-drawer-test ()
  (should (equal (org-expenses/hl--has-property-drawer (org-expenses/get-headline "Sonstiges"))
                 nil))
  (should (equal (org-expenses/hl--has-property-drawer (org-expenses/get-headline "Bücher"))
                 nil))
  (should (equal (org-expenses/hl--has-property-drawer (org-expenses/get-headline "Buch1"))
                 t))
  (should (equal (org-expenses/hl--has-property-drawer (org-expenses/get-headline "Pizza"))
                 t)))

(ert-deftest org-expenses/hl--get-hl-path-test ()
  (should (equal (org-expenses/hl--get-hl-path (org-expenses/get-headline "Sonstiges"))
                 '("Sonstiges")))
  (should (equal (org-expenses/hl--get-hl-path (org-expenses/get-headline "Pizza"))
                 '("Sonstiges" "Pizza"))))

(ert-deftest org-expenses/filter--currencies-test ()
  (should (equal (org-expenses/filter--currencies '(:a 1 :b 2))
                 nil))
  (should (equal (org-expenses/filter--currencies '(:item "test" :usd 12.99))
                 '(:USD))))

(ert-deftest org-expenses/hl--to-item-test ()
  (should (equal (org-expenses/hl--to-item (org-expenses/get-headline "Buch1") "testfile.org")
                 `(:tags ("book") :level 3 :item "Buch1" :begin 99
                   :filename ,(abbreviate-file-name "testfile.org")
                   :path ("Sonstiges" "Bücher") :category "Bücher"
                   :EUR 15.99 :date "[2014-08-31 So]"))))

(ert-deftest org-expenses/collect--props-test ()
  (let* ((items (org-expenses/collect--props (org-expenses/parse--file "test-exp3.org")
                                        "test-exp3.org"
                                        'identity))
         (meta (car items)))
    (should (equal (length items) 2))
    (should (equal (plist-get (cadr items) :EUR) 10.0))
    (should (equal (plist-get (cadr items) :CHF) nil))
    (should (equal (plist-member (cadr items) :CHF) '(:CHF nil))))
  (let* ((items (org-expenses/collect--props (org-expenses/parse-test-file)
                                        "test-exp1.org"
                                        'identity))
         (meta  (car items)))
    (should (equal (length items) 8))
    (should (equal (plist-get meta :truncated) nil))
    (should (equal (plist-get meta :size) 7)))
  (let* ((items (org-expenses/collect--props (org-expenses/parse-test-file)
                                        "test-exp1.org"
                                        'identity
                                        2))
         (meta  (car items)))
    (should (equal (length items) 3))
    (should (equal (plist-get meta :truncated) t))
    (should (equal (plist-get meta :size) 2))))

(ert-deftest org-expenses/property-override-test ()
  (let* ((items (org-expenses/collect--props
                 (org-expenses/parse-string
                  "* not a category\n
** my item\n
   :PROPERTIES:
   :chf: 12.44
   :CATEGORY: fish
   :END:")
                 "none"
                 'identity))
         (cat (plist-get (cadr items) :category)))
    (should (equal cat "fish")))
  (let* ((items (org-expenses/collect--props
                 (org-expenses/parse-string
                  "* not a category\n
** my item\n
   :PROPERTIES:
   :chf: 12.44
   :item: another item
   :END:")
                 "none"
                 'identity))
         (cat (plist-get (cadr items) :item)))
    (should (equal cat "another item"))))

(ert-deftest org-expenses/filter-tags-test ()
  (let ((filter (org-expenses/filter-tags "-book")))
    (should (equal (funcall filter (org-expenses/get-item "Buch1"))
                   nil))
    (should (equal (funcall filter (org-expenses/get-item "Pizza"))
                   t)))
  (let ((filter (org-expenses/filter-tags "restaurant")))
    (should (equal (funcall filter (org-expenses/get-item "Buch1"))
                   nil))
    (should (equal (funcall filter (org-expenses/get-item "Pizza"))
                   t)))
  (let ((filter (org-expenses/filter-tags '("telecom" "-gadget"))))
    (should (equal (funcall filter (org-expenses/get-item "Cellphone"))
                   nil))
    (should (equal (funcall filter (org-expenses/get-item "Cablecom"))
                   t))))

(ert-deftest org-expenses/filter-category-test ()
  (let ((filter (org-expenses/filter-category "Bücher")))
    (should (equal (funcall filter (org-expenses/get-item "Buch1")) t))
    (should (equal (funcall filter (org-expenses/get-item "Pizza")) nil))))

(ert-deftest org-expenses/filter-item-name-test ()
  (let ((filter (org-expenses/filter-item-name "Pizza")))
    (should (equal (funcall filter (org-expenses/get-item "Pizza")) t))
    (should (equal (funcall filter (org-expenses/get-item "Buch1")) nil))))

(ert-deftest org-expenses/filter-item-regexp-test ()
  (let ((filter (org-expenses/filter-item-regexp "^C.*")))
    (should (equal (funcall filter (org-expenses/get-item "Pizza")) nil))
    (should (equal (funcall filter (org-expenses/get-item "Cablecom")) 0))
    (should (equal (funcall filter (org-expenses/get-item "Cellphone")) 0))))

(ert-deftest org-expenses/filter-date-test ()
  (let ((filter (org-expenses/filter-date "2014-08")))
    (should (equal (funcall filter (org-expenses/get-item "Pizza")) t))
    (should (equal (funcall filter (org-expenses/get-item "Buch1")) t)))
  (let ((filter (org-expenses/filter-date "2014-08-28--2014-08-31")))
    (should (equal (funcall filter (org-expenses/get-item "Pizza")) t))
    (should (equal (funcall filter (org-expenses/get-item "Cellphone")) t))
    (should (equal (funcall filter (org-expenses/get-item "Buch2")) nil))))

(ert-deftest org-expenses/make-search-filter-test ()
  (let* ((searchobj '(:tags "telecom" :date "2014-08"))
         (filter1   (org-expenses/make-search-filter searchobj))
         (filter2   (org-expenses/make--filter (list (org-expenses/filter-tags "telecom")
                                                (org-expenses/filter-date "2014-08"))))
         (items     (org-expenses/collect--props (org-expenses/parse-test-file)
                                            "test-exp1.org"
                                            'identity)))
    (-each items
      (lambda (el)
        (should (equal (funcall filter1 el) (funcall filter2 el)))))))

(defun org-expenses/search-files-test (searchfun)
  (let* ((items (funcall searchfun
                         '(:tags "telecom")
                         (-list "test-exp1.org")))
         (names (-map (lambda(el) (plist-get el :item)) (cdr items))))
    (should (equal (plist-get (car items) :truncated) nil))
    (should (equal (length items) 4))
    (should (equal (plist-get (car items) :size) 3))
    (should (equal (length (-distinct names)) 2))
    (should (equal (-sort 'string-lessp names) '("Cablecom" "Cablecom" "Cellphone"))))
  (let ((items (funcall searchfun
                        '(:tags "telecom")
                        (-list "test-exp1.org"))))
    (should (equal (length items) 4))
    (should (equal (plist-get (car items) :truncated) nil))
    (should (equal (plist-get (car items) :size) 3)))
  (let ((items (funcall searchfun
                        '(:tags "telecom")
                        (-list "test-exp1.org")
                        2)))
    (should (equal (length items) 3))
    (should (equal (plist-get (car items) :truncated) t))
    (should (equal (plist-get (car items) :size) 2)))
  (let ((items (funcall searchfun
                        '(:tags "telecom")
                        (-list "test-exp1.org" "test-exp2.org")
                        6)))
    (should (equal (length items) 7))
    (should (equal (plist-get (car items) :truncated) nil))
    (should (equal (plist-get (car items) :size) 6))))

(ert-deftest org-expenses/search-buffers-test ()
  (org-expenses/search-files-test 'org-expenses/search-buffers))

(ert-deftest org-expenses/sqlite--convert-results-test ()
  (with-temp-buffer
    (insert "   id = 2  \n")
    (insert "  name = \"john\" \n\n")
    (insert " id = 5\n")
    (insert "      name=\"mary\"")
    (should (equal (org-expenses/sqlite--convert-results (current-buffer))
                   '((:id 2 :name "john") (:id 5 :name "mary")))))
  (with-temp-buffer
    (insert "   id = 2\n")
    (insert "  name = \"john\"\n\n")
    (insert " id = 5\n")
    (insert "      name=\"mary\"")
    (should (equal (org-expenses/sqlite--convert-results (current-buffer))
                   '((:id 2 :name "john") (:id 5 :name "mary")))))
  (with-temp-buffer
    (insert "   id = 2")
    (should (equal (org-expenses/sqlite--convert-results (current-buffer))
                   '((:id 2)))))
  (with-temp-buffer
  (should (equal (org-expenses/sqlite--convert-results (current-buffer))
                 nil)))
  (with-temp-buffer
    (insert "id = ")
    (should (equal (org-expenses/sqlite--convert-results (current-buffer))
                   '((:id nil)))))
  (with-temp-buffer
    (insert "  id = 2 \n")
    (insert " name =\n")
    (should (equal (org-expenses/sqlite--convert-results (current-buffer))
                   '((:id 2 :name nil))))))

(ert-deftest org-expenses/with-sqlite-buffer-test ()
  (when (get-buffer "*sqlite-log*")
    (kill-buffer "*sqlite-log*"))
  (unwind-protect
      (org-expenses/with-sqlite-buffer ".print test" "x.db"
        (should (equal (not (get-buffer "*sqlite-log*")) nil))
        (should (equal (buffer-string) "test\n"))
        (should (equal (with-current-buffer "*sqlite-log*" (buffer-string))
                       "
> Execute: .print test
  0 success.\n")))
    (org-expenses/f-delete "x.db")
    (kill-buffer "*sqlite-log*")))

(ert-deftest org-expenses/sqlite-exec-test ()
  (should (equal (org-expenses/sqlite-exec ".print test" "x.db") t)))

(ert-deftest org-expenses/sqlite-exec-lines ()
  (unwind-protect
      (let ((lst (org-expenses/sqlite-exec-lines ".help" "x.db")))
        (should (equal (listp lst) t))
        (should (< 50 (length lst))))
    (org-expenses/f-delete "x.db")))

(ert-deftest org-expenses/sqlite-select-test ()
  (unwind-protect
      (progn
        (org-expenses/sqlite-exec "create table person (name text, age integer)"
                             "x.db")
        (org-expenses/sqlite-exec "insert into person (name,age) values ('\"john\"', 20)"
                             "x.db")
        (org-expenses/sqlite-exec "insert into person (name,age) values ('\"mary\"', 21)"
                             "x.db")
        (let ((results (org-expenses/sqlite-select
                        "select name, age from person order by name"
                        "x.db")))
          (should (equal results '((:name "john" :age 20)
                                   (:name "mary" :age 21))))))
    (org-expenses/f-delete "x.db")))

(ert-deftest org-expenses/sqlite--table-p-test ()
  (unwind-protect
      (progn
        (org-expenses/sqlite-exec "create table person (name text, age integer)"
                             "x.db")
        (should (equal (org-expenses/sqlite--table-p "person" "x.db") t))
        (should (equal (org-expenses/sqlite--table-p "other" "x.db") nil)))
    (org-expenses/f-delete "x.db")))

(ert-deftest org-expenses/sqlite--column-p-test ()
  (unwind-protect
      (progn
        (org-expenses/sqlite-exec "create table person (name text, age integer)"
                             "x.db")
        (should (equal (org-expenses/sqlite--column-p "person" "name" "x.db") t))
        (should (equal (org-expenses/sqlite--column-p "person" "nr" "x.db") nil)))
    (org-expenses/f-delete "x.db")))

(ert-deftest org-expenses/sqlite--file-imported-p-test ()
  (org-expenses/with-db "z.db"
    (should (equal (org-expenses/sqlite--file-imported-p "test-exp1.org") nil))
    (org-expenses/sqlite-exec "create table files (name text, time integer)" "z.db")
    (org-expenses/sqlite-exec "insert into files (name, time) values ('test-exp1.org', 1000)"
                               "z.db")
    (should (equal (org-expenses/sqlite--file-imported-p "test-exp1.org") nil))
    (org-expenses/sqlite-exec (format "update files set time = %d"
                                      (org-expenses/file-last-mod-sec "test-exp1.org"))
                              "z.db")
    (should (equal (org-expenses/sqlite--file-imported-p "test-exp1.org") t))))

(ert-deftest org-expenses/sqlite--make-column-name-test ()
  (should (equal (org-expenses/sqlite--make-column-name "age") "\"age\""))
  (should (equal (org-expenses/sqlite--make-column-name :test) "\"test\"")))

(ert-deftest org-expenses/sqlite--make-value-string-test ()
  (should (equal (org-expenses/sqlite--make-value-string "pizza") "'\"pizza\"'"))
  (should (equal (org-expenses/sqlite--make-value-string 1) "1"))
  (should (equal (org-expenses/sqlite--make-value-string (list "pizza" "hot"))
                 "'(\"pizza\" \"hot\")'"))
  (should (equal (org-expenses/sqlite--make-value-string "it's") "'\"it''s\"'"))
  (should (equal (org-expenses/sqlite--make-value-string (list "it's" "he's"))
                 "'(\"it''s\" \"he''s\")'")))

(ert-deftest org-expenses/sqlite--collect-columns-test ()
  (let ((items (org-expenses/collect--props (org-expenses/parse-test-file)
                                       "testfile.org"
                                       'identity)))
    (should (equal (org-expenses/sqlite--collect-columns items)
                   `(:tags ("book") :level 3 :item "Buch1" :begin 99
                     :filename ,(abbreviate-file-name "testfile.org")
                     :path ("Sonstiges" "Bücher") :category "Bücher" :EUR 15.99
                     :date "[2014-08-31 So]" :CHF 20.0)))))


(ert-deftest org-expenses/sqlite--make-insert-test ()
  (should (equal (org-expenses/sqlite--make-insert (org-expenses/get-item "Buch1"))
                 (concat "insert into expenses (\"date\", \"EUR\", \"category\", "
                         "\"path\", \"filename\", \"begin\", \"item\", \"level\", "
                         "\"tags\") values ('\"[2014-08-31 So]\"', 15.99, '\"Bücher\"', "
                         "'(\"Sonstiges\" \"Bücher\")', '\"" (abbreviate-file-name "test-exp1.org")
                         "\"', 99, '\"Buch1\"', 3, '(\"book\")')"))))

(ert-deftest org-expenses/sqlite--adjust-columns-test ()
    (unwind-protect
        (should (equal (-sort 'string-lessp
                              (org-expenses/sqlite--adjust-columns
                               "expenses"
                               '(:level 3 :item "Buch1" :begin 99
                                        :path ("Sonstiges" "Bücher")
                                        :EUR 15.99 :date "[2014-08-31 So]")
                               "z.db"))
                       (-sort 'string-lessp
                              '("alter table expenses add column \"date\" text"
                                "alter table expenses add column \"EUR\" numeric"
                                "alter table expenses add column \"path\" text"
                                "alter table expenses add column \"begin\" integer"
                                "alter table expenses add column \"item\" text"
                                "alter table expenses add column \"level\" integer"))))
      (org-expenses/f-delete "z.db")))

(ert-deftest org-expenses/sqlite--import-test ()
  (org-expenses/with-db "t2.db"
    (org-expenses/sqlite--init-schema)
    (org-expenses/sqlite--import "test-exp1.org")
    (should (equal (plist-get (car (org-expenses/sqlite-select
                                    "select count(*) as count from expenses"
                                    "t2.db")) :count)
                   7)))
  (org-expenses/with-db "t3.db"
    (let ((r1 (org-expenses/sqlite-search '(:date "2014-08")
                                    '("test-exp1.org")))
          (r2 (org-expenses/search-buffers '(:date "2014-08")
                                      '("test-exp1.org"))))
      (should (org-expenses/plist-equal (car r1) (car r2)))))
  (org-expenses/with-db "t3.db"
    (let ((r1 (org-expenses/sqlite-search '(:item "Cellphone")
                                     '("test-exp1.org")))
          (r2 (org-expenses/search-buffers '(:item "Cellphone")
                                      '("test-exp1.org"))))
      (should (org-expenses/plist-equal (car r1) (car r2)))
      (should (org-expenses/plist-equal (nth 1 r1) (nth 1 r2))))))


(ert-deftest org-expenses/sqlite-search-test ()
  (org-expenses/with-db "t4.db"
    (org-expenses/search-files-test 'org-expenses/sqlite-search)))

(ert-deftest org-expenses/group-results-test ()
  (let ((res (org-expenses/group-results
              (org-expenses/search-buffers nil '("test-exp1.org"))
              'org-expenses/group-by-month)))
    (should (equal 2 (length res)))
    (should (equal 7 (length (cdr (car (cdr res)))))))
  (let ((res (org-expenses/group-results
              (org-expenses/search-buffers nil '("test-exp1.org"))
              'org-expenses/group-by-category)))
    (should (equal 4 (length res)))
    (should (equal 3 (length (cdr (assoc "Bücher" (cdr res))))))
    (should (equal 1 (length (cdr (assoc "Work" (cdr res))))))
    (should (equal 3 (length (cdr (assoc "Sonstiges" (cdr res))))))))

(ert-deftest org-expenses/custom-date-property-test ()
  (let* ((org-expenses/date-property :datum)
         (items (org-expenses/collect--props
                 (org-expenses/parse-string
                  "* not a category
** my item
   :PROPERTIES:
   :chf: 12.44
   :datum: [2014-09-14]
   :END:")
                 "none"
                 'identity)))
    (should (equal (plist-get (cadr items) :date)
                   "[2014-09-14]"))))

(ert-deftest org-expenses/get-date-from-somewhere-test ()
  (let* ((items (org-expenses/collect--props
                 (org-expenses/parse-string
                  "* not a category
** my item
   :PROPERTIES:
   :chf: 12.44
   :END:

   [2014-09-16]")
                 "none"
                 'identity)))
    (should (equal (plist-get (cadr items) :date)
                   "[2014-09-16]"))))

(ert-deftest org-expenses/negative-values-test ()
  (let* ((items (org-expenses/collect--props
                 (org-expenses/parse-string
                  "* not a category
** my item 1
   :PROPERTIES:
   :chf: 12.44
   :END:
** my item 2
   :PROPERTIES:
   :chf: -10.44
   :END: ")
                 "none"
                 'identity)))
    (should (equal 12.44 (plist-get (cadr items) :CHF)))
    (should (equal -10.44 (plist-get (caddr items) :CHF)))
    (let ((sum (plist-get (org-expenses/summarize-results items) :CHF)))
      (should (equal 2.0 (plist-get sum :sum)))
      (should (equal 1.0 (plist-get sum :avg)))
      (should (equal 12.44 (plist-get sum :max)))
      (should (equal -10.44 (plist-get sum :min))))))
