;;; org-expenses.el --- track expenses with org  -*- lexical-binding: t -*-

;; Copyright © 2014 Eike Kettner

;; Package-Requires: ((dash "2.8.0") (s "1.9.0"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Tracking expenses in [[http://orgmode.org][org]] files works very
;; well. The org capture mechanism can be used to quickly insert
;; them. This library provides utility functions to generate basic
;; statistics from expense lists.
;;
;; It assumes the following conventions:
;;
;; - expense items are headings, to be able to apply tags and
;; - properties where one is a currency code - price pair
;;
;; An expense item is a headline that complies to those two
;; points. Expense items can have one category, but in contrast to
;; org's property ~CATEGORY~, an expense category is just the name of
;; the parent headline.
;;
;; Then those items are gathered from some org files and org tables
;; are generated to create an overview.
;;
;; See the README for more information.
;;
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-table)
(require 'calendar)
(require 'dash)
(require 's)
(require 'thingatpt)

(defvar org-expenses/files "~/org/expenses/"
  "Either a list of file names or a directory.
If a directory is specified, all .org files are considered.")

(defvar org-expenses/sqlite-cmd "/usr/bin/sqlite3"
  "The sqlite3 executable.")

(defvar org-expenses/date-property :date
  "The property key used to specify the date of an expense item in your files.
Your expense item headlines should have such a property.  It must
be convertable into a keyword.")

(defvar org-expenses/sqlite-db-file nil
  "The database file to use with sqlite.
If this is nil, sqlite is not used.")

(defvar org-expenses/currency-list '(:USD :EUR :CHF :CZK
                                     :DKK :GBP :RUB :SEK :JPY)
  "A list of currency codes.
Only headlines that have one of those as property keys are
considered 'expense items'.")

(defvar org-expenses/max-results nil
  "Limit search results to some maximum.")

(defvar org-expenses/search-filters
  '(:tags org-expenses/filter-tags
    :category org-expenses/filter-category
    :item org-expenses/filter-item-regexp
    :date org-expenses/filter-date)
  "A property list mapping search keys to filter factory functions.

The filter factory functions are expected to return a function
that filters result items. The factory functions are called with
some value to search for. This is used with
`org-expenses/make-search-filter' to construct a set of functions from
a given search object.

A search object is a plist with keys from this list and a value
to search.")

(defvar org-expenses/log-sqlite-output nil
  "If t log all sqlite output to *sqlite-log* buffer.
By default only error output is logged.")

(defvar org-expenses/sqlite-import-batchsize 70
  "Number of insert statements send to sqlite at once.")

(defvar org-expenses/overview-tables
  '(("** Summary" . org-expenses/summary-table)
    ("** Categories" . (lambda (results)
                         (org-expenses/summary-table
                          results
                          'org-expenses/group-by-category)))
    ("** Monthly" . (lambda (results)
                      (org-expenses/summary-table
                       results
                       'org-expenses/group-by-month))))
  "A alist of name function pairs, where the function creates a
  table from a result list.")

(defvar org-expenses/item-columns '(:item :category :date)
  "A list of columns to use in items table.")


(defun org-expenses/file-list (&optional silent)
  "Make a list of all expense files to consider.
This interprets the variable `org-expenses/files'."
  (if (listp org-expenses/files)
      (delq nil
            (-map (lambda (fn)
                    (let ((existing (file-exists-p fn)))
                      (when (and (not existing)
                                 (or silent
                                     (yes-or-no-p (format
                                                   "File %s does not exist. Abort? " fn))))
                        (error "Abort because a file is missing."))
                      (if existing (abbreviate-file-name fn) nil)))
                  org-expenses/files))
    (unless (stringp org-expenses/files)
      (error "org-expenses/files must be a string or a list of files"))
    (when (file-directory-p org-expenses/files)
      (-map 'abbreviate-file-name (directory-files org-expenses/files t "^[^\\.].*?\\.org$")))))



;; ---- utilities

(defun org-expenses/sort-keywords (lst)
  "Sort the list of keywords LST and remove duplicates."
  (-sort (lambda (a b) (string-lessp (symbol-name a) (symbol-name b)))
         (-distinct lst)))

(defun org-expenses/plist-map (plist fun &optional flatten)
  "Map FUN over the key value pairs of the given PLIST.
The plist may contain multiple such pairs. FUN is expected to take
two arguments: the key and value. A list of the collected results
is returned, but without nil values and in reverse order."
  (let ((lst plist)
        (res '()))
    (while lst
      (let* ((fst (car lst))
             (sec (cadr lst))
             (val (funcall fun fst sec)))
        (unless (null val)
          (if (and flatten (or (vectorp val) (listp val)))
              (setq res (append val res))
            (setq res (cons val res)))))
      (setq lst (cddr lst)))
    res))

(defun org-expenses/plist-kmap (plist fun &optional flatten)
  "Map FUN over all keys in the plist PLIST."
  (org-expenses/plist-map plist
                     (lambda (k _v)
                       (funcall fun k))
                     flatten))

(defun org-expenses/plist-remove (plist key)
  "Remove the key-value pair from PLIST whose key is KEY."
  (org-expenses/plist-map plist
                     (lambda (k v)
                       (unless (equal k key)
                         (list k v)))
                     t))

(defun org-expenses/string-number-p (str)
  "Check whether STR represents a number.
Return t if STR only contains digits and one optional dot."
  (when (and (stringp str)
             (string-match "^-?[0-9]*\\.?[0-9]*$" str))
    t))

(defun org-expenses/string-or-number (str)
  "Convert STR into a number.
Convert if STR is a number according to
`org-expenses/string-number-p', otherwise return STR."
  (if (org-expenses/string-number-p str)
      (string-to-number str) str))

(defun org-expenses/as-number (obj)
"Try to convert OBJ into a number or return 0."
  (cond ((null obj) 0)
        ((numberp obj) obj)
        ((org-expenses/string-number-p obj) (string-to-number obj))
        (t 0)))

(defun org-expenses/n+ (&rest args)
  "A more lenient + function.  It also attempts to convert
strings into numbers and assumes 0 for nil and other unkown
values."
  (if (null args) 0
    (apply '+ (-map 'org-expenses/as-number args))))


(defun org-expenses/get-currency (curr)
  "This normalises CURR into a currency keyword if it matches one
in `org-expenses/currency-list'. CURR maybe a string or a keyword, case
insensitiv. A keyword from `org-expenses/currency-list' is returned if CURR
matches, otherwise nil."
  (cond
   ((keywordp curr) (org-expenses/get-currency (symbol-name curr)))
   ((stringp curr) (let ((name (upcase curr)))
                     (car (memq (intern
                                 (if (s-starts-with-p ":" name)
                                     name (concat ":" name)))
                                org-expenses/currency-list))))
   (t nil)))


(defun org-expenses/as-string (obj)
  "Make OBJ a string.
If OBJ is a keyword, remove the colon."
  (cond
   ((keywordp obj)
    (substring (symbol-name obj) 1))
   ((numberp obj)
    (number-to-string obj))
   ((stringp obj)
    obj)
   (t (error "No string for %s" (prin1-to-string obj)))))



(defun org-expenses/format-price (price &optional currency)
  (cond
   ((or (null price) (eq price 0)) "")
   ((org-expenses/string-number-p price)
    (org-expenses/format-price (string-to-number price)
                               currency))
   ((numberp price)
    (concat (if currency
                (concat (org-expenses/as-string currency) " ")
              "")
            (format "%.2f" price)))
   (t "")))


;; not using `org-parse-time-string', because need to recognize
;; partial dates, too.
(defun org-expenses/parse-date-parts (str)
  "Parses a (potentially partial) date string into its
components. Remove invalid chars from STR first."
  (unless (null str)
    (mapcar 'string-to-number
            (split-string
             (replace-regexp-in-string "[^0-9\-]"
                                       ""
                                       str) "-"))))

(defun org-expenses/expand-date (str &optional beg-or-end)
  "Takes a partial date string, like '2014-08' and returns a full
date string either at the start or the end of the most specific
part. The BEG-OR-END can be either :start or :end (default).

Example:
  (equal (org-expenses/expand-date \"2014-01\" :start) \"[2014-01-01]\")
  (equal (org-expenses/expand-date \"2014-02\" :end) \"[2014-02-28]\")"
  (let* ((date (org-expenses/parse-date-parts str))
         (startp (eq beg-or-end :start))
         (year  (car date))
         (month (or (cadr date) (if startp 1 12)))
         (day   (or (nth 2 date) (if startp 1 (calendar-last-day-of-month month year)))))
    (format "[%4d-%02d-%02d]" year month day)))

(defun org-expenses/abbrev-date (date)
  "Abbreviate a date string DATE.
The reverse of `org-expenses/expand-date'. If DATE is beginning or end
of a month/year, then it is abbreviated. Example:

  (equal (org-expenses/abbrev-date \"2014-01\") \"2014\")
  (equal (org-expenses/abbrev-date \"2014-01-01\") \"2014\")
  (equal (org-expenses/abbrev-date \"2014-01-31\") \"2014\")
  (equal (org-expenses/abbrev-date \"2014-07-31\") \"2014-07\") "
  (when (s-contains-p "--" date)
    (user-error "abbrev-date does not work with ranges"))
  (let ((parts (org-expenses/parse-date-parts date)))
    (cond
     ((= 1 (length parts)) date)
     ((= 2 (length parts))
      (if (or (= 1 (nth 1 parts))
              (= 12 (nth 1 parts)))
          (format "%d" (car parts))
        date))
     ((= 3 (length parts))
      (let ((year (car parts))
            (month (nth 1 parts))
            (day (nth 2 parts)))
        (if (or (= 1 day)
                (<= (calendar-last-day-of-month month year)
                   day))
            (org-expenses/abbrev-date (format "%d-%02d" year month))
          date))))))


(defun org-expenses/expand-date-range (date)
  "Returns a string with the beginning and end of the DATE. If
DATE is a complete date string, like '2014-08-22' (brackets are
not required), then this date is returned as beginning and
end. If it is a partial date, like '2014-07', then a beginning
and end is created from the most specific component given."
  (let ((range (if (null date) nil (split-string date "--"))))
    (when (> (length range) 2)
      (user-error "Invalid date range: %s" date))
    (unless (null range)
      (let ((beg (org-expenses/expand-date (car range) :start))
            (end (org-expenses/expand-date (if (= 1 (length range))
                                          (car range)
                                        (cadr range))
                                      :end)))
        (if (org-time<= beg end)
            (format "%s--%s" beg end)
          (user-error "Invalid date range: %s" date))))))


(defun org-expenses/abbrev-date-range (range)
  "Abbreviates a date range string.
The reverse of `org-expenses/expand-date-range'."
  (let ((split (split-string range "--" t)))
    (if (= 1 (length split))
        (org-expenses/abbrev-date (car split))
      (let ((amin (org-expenses/abbrev-date (car split)))
            (amax (org-expenses/abbrev-date (cadr split))))
        (if (s-equals-p amin amax)
            amin
          (format "%s--%s" amin amax))))))

(defun org-expenses/date-range-tuple (date)
  (let ((range (split-string
                (org-expenses/expand-date-range date)
                "--" t)))
    (when (org-time<= (car range) (cadr range))
      (cons (car range) (cadr range)))))

(defun org-expenses/date-range-p (daterange)
  "Return whether DATERANGE is a valid date range string."
  (condition-case nil
      (progn (org-expenses/expand-date-range daterange) t)
    (error nil)))

(defun org-expenses/date-in-range-p (range date)
  "RANGE must be obtained via `expand-date-to-range' and DATE
must be a valid org date string. Returns t if DATE is within
RANGE (inclusive start and end)."
  (let* ((dater (org-expenses/date-range-tuple range))
         (start (car dater))
         (end   (cdr dater)))
    (when (and (org-time>= end date)
               (org-time<= start date))
      t)))

(defun org-expenses/date--range-simple-add (range n)
  "Return RANGE string with N added to it.

RANGE must be an abbreviated date range string (i.e. without
'--'), it adds N to the most specific part of RANGE."
  (let* ((parts (org-expenses/parse-date-parts range))
         (year (car parts))
         (month (nth 1 parts))
         (day (nth 2 parts)))
    (cond
     (day
      (let ((date (date-to-time (format "%d-%02d-%02d 0:00:00" year month day)))
            (days (days-to-time (abs n))))
        (format-time-string "[%Y-%m-%d]"
                            (if (< n 0)
                                (time-subtract date days)
                              (time-add date days)))))
     (month
      (let* ((num (+ month (* 12 year)))
             (next (+ num n))
             (y (/ next 12))
             (m (% next 12)))
        (if (= 0 m)
            (format "%d-%02d" (- y 1) 12)
          (format "%d-%02d" y m))))
     (t (number-to-string (+ year n))))))

(defun org-expenses/date-range-move (range n)
"Move RANGE by N.

The most specific part of the range is used as operand to N. For
example, if RANGE is '2014-08' and N is 1, then '2014-09' is
returned. RANGE can be an expanded range string, always both
sides are affected."
  (let ((r (s-split "--" range t)))
    (if (= 2 (length r))
        (format "%s--%s"
                (org-expenses/date--range-simple-add (car r)
                                                n)
                (org-expenses/date--range-simple-add (cadr r)
                                                n))
      (org-expenses/date--range-simple-add range n))))


(defun org-expenses/date-range-extend (range n)
  "Extend RANGE by N.
The RANGE is widened or narrowed depending on N being positiv or
negativ. N is applied to the 'end' part of the range."
  (unless (org-expenses/date-range-p range)
    (user-error "Invalid date range: %s" range))
  (let* ((interval (if (s-contains-p "--" range)
                      (s-split "--" range)
                    (list range range)))
         (extdate (format "%s--%s"
                          (car interval)
                          (org-expenses/date--range-simple-add
                           (cadr interval) n))))
    (if (org-expenses/date-range-p extdate)
        (org-expenses/abbrev-date-range extdate)
      (let* ((parts (org-expenses/parse-date-parts (cadr interval)))
             (len (length parts))
             (year (car parts))
             (month (nth 1 parts)))
        (cond
         ((= 1 len)
          (org-expenses/date-range-extend
           (format "%s--%d-12" (car interval) year)
           n))
         ((= 2 len)
          (org-expenses/date-range-extend
           (format "%s--%d-%02d-%02d"
                   (car interval)
                   year month
                   (calendar-last-day-of-month month year))
           n))
         (t range))))))


;; --- parsing and converting


(defun org-expenses/parse--file (file)
  "Uses `org-element-parse-buffer' to parse FILE into an ast.

FILE can be a file or a buffer."
  (if (bufferp file)
      (with-current-buffer file
        (message "Parse current buffer for expense items…")
        (org-element-parse-buffer))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (message "Parse file '%s' for expense items…" (abbreviate-file-name file))
      (org-element-parse-buffer))))

(defun org-expenses/make--filter (filters)
  "Makes a filter function out of FILTERS.  Multiple filter
functions are and-ed by default, but this can be changed by
specifying the first element of FILTERS to be one `:or', `:and'
or `:not' (`:and' is default). If `:not' is specified each single
filter output is negated, so it's like '(not (or FILTERS))'."
  (if (-some-p 'functionp filters)
      (let* ((mfilters (if (functionp (car filters)) filters (cdr filters)))
             (fst   (car filters))
             (junct (cond ((eq :or  fst) '-some-p)
                          ((eq :not fst) '-some-p)
                          (t '-all-p)))
             (neg   (if (eq :not fst) 'not 'identity)))
        (lambda (el)
          (funcall neg
                   (funcall junct (lambda (f)
                                    (funcall f el))
                            mfilters))))
    'identity))

(defun org-expenses/hl--has-property-drawer (hl)
  "Returns t if the given headline has itself a property drawer
attached; otherwise nil."
  (let ((content (org-element-contents hl)))
    (when (memq 'property-drawer
                (-mapcat (lambda (el)
                           (when (eq 'section (org-element-type el))
                             (mapcar 'org-element-type el)))
                         content))
      t)))

(defun org-expenses/hl--get-hl-path (hl)
  "Returns the path as a list from HL to the top."
  (if (null hl) nil
    (let ((name (org-element-property :raw-value hl))
          (parent (org-element-property :parent hl)))
      (reverse
       (if (null name)
           (org-expenses/hl--get-hl-path parent)
         (cons name (org-expenses/hl--get-hl-path parent)))))))

(defun org-expenses/filter--currencies (el)
  "Function used inside `org-expenses/collect--props' to keeps
only those entries whose properties have one of the currencies
defined in `org-expenses/currency-list'."
  (org-expenses/plist-kmap el 'org-expenses/get-currency))

(defun org-expenses/hl--to-item (hl mfile)
  "Converts an org headline into a p-list."
  (let ((props
         ;; -mapcat 'identity, because -flatten removes all nils,
         ;; which destroys plist structure
         (-mapcat 'identity
                  (org-element-map hl 'node-property
                    (lambda (np)
                      (let* ((key (intern
                                   (concat ":"
                                           (downcase (org-element-property :key np)))))
                             (ckey (org-expenses/get-currency key))
                             (value (org-element-property :value np)))
                        (if ckey
                            (list ckey (if (or (null value) (s-equals-p value ""))
                                           nil
                                         (org-expenses/string-or-number value)))
                          (if (equal key org-expenses/date-property)
                              (list :date (org-expenses/string-or-number value))
                            (list key (org-expenses/string-or-number value)))))))))
        (defaults
          (list :tags (org-element-property :tags hl)
                :level (org-element-property :level hl)
                :item (org-element-property :raw-value hl)
                :begin (org-element-property :begin hl)
                :filename (abbreviate-file-name mfile)
                :path (org-expenses/hl--get-hl-path
                       (org-element-property :parent hl))
                :category (org-element-property :raw-value
                                                (org-element-property :parent hl)))))
    (org-expenses/plist-map props
      (lambda (k v)
        (setq defaults
              (plist-put defaults k v))
        nil))
    (unless (plist-get defaults :date)
      (let ((ts (org-element-map hl 'timestamp
                  (lambda (ts) (org-element-property :raw-value ts))
                  nil t)))
        (when ts
            (setq defaults
                  (plist-put defaults :date ts)))))
    defaults))

(defun org-expenses/collect--props (tree filename filter &optional maxsize)
  "Filter the org ast TREE and convert headlines to plists.

Each headline with a property drawer and an existing currency
property key is converted into a plist of all its properties (and
tags, level, name and parent headline). All those that pass
FILTER are collected into a list and returned. The TREE argument
is the output of `org-element-parse-buffer', FILENAME is the name
of the file corresponding to TREE and FILTER a filter
function. Optional MAXSIZE may be specified that will truncate
the result list if appropriate.

It returns a list of plists where the first item contains meta
information, the results start with the second item."
  (let* ((maxsz   (if maxsize (1+ maxsize) nil))
         (trunc   nil)
         (counter 0)
         (items (org-element-map tree 'headline
                  (lambda (hl)
                    (when (and (or (null maxsz) (<= counter maxsz))
                               (not trunc)
                               (org-expenses/hl--has-property-drawer hl))
                      (let ((item (org-expenses/hl--to-item hl filename)))
                        (when (and (org-expenses/filter--currencies item)
                                   (funcall filter item))
                          (setq counter (1+ counter))
                          (if (eq counter maxsz)
                              (progn (setq trunc t)
                                     (setq counter (1- counter))
                                     nil)
                            item))))))))
    (cons (list :size counter :maxsize maxsz :truncated trunc)
          items)))


;; ---- filter functions
;;

(defun org-expenses/dissect--char-prefix (str)
  "Dissects STR potentially prefixed by a - or + character.

Return a list with three elements:
1. the prefix, either nil, + or -
2. the value, either STR or STR without first char
3. a function: `not' if prefix is -, otherwise `identity'"
  (let* ((char (string-to-char (substring str 0 1)))
         (prefix (if (or (eq ?- char) (eq ?+ char)) char nil))
         (negatefun (if (eq ?- prefix) 'not 'identity))
         (val    (if prefix (substring str 1) str)))
    (list prefix val negatefun)))

(defun org-expenses/filter-member (key values)
  "Generic filter function factory that checks whether VALUES and
the values of the property KEY intersect. VALUES maybe a string
or a list thereof. VALUES elements can be prefixed with either +
or - which means whether to check for inclusion or exclusion,
respectively. "
  (let ((mvalues (-list values)))
    (lambda (el)
      (let ((elvalues (-list (plist-get el key))))
        (-all-p
         (lambda (val)
           (let* ((dissect (org-expenses/dissect--char-prefix val))
                  (negate  (nth 2 dissect))
                  (rawval  (nth 1 dissect)))
             (funcall negate (member rawval elvalues))))
         mvalues)))))

(defun org-expenses/filter-tags (tags)
  "This creates a new function that keeps those entries that have
one of TAGS."
  (org-expenses/filter-member :tags tags))

(defun org-expenses/filter-category (cats)
  "This creates a filter function that keeps those entries whose
:category is in CATS."
  (org-expenses/filter-member :category cats))

(defun org-expenses/filter-item-name (name)
  "Creates a filter function which keeps entries with item name
NAME."
  (org-expenses/filter-member :item name))

(defun org-expenses/filter-item-regexp (regexp)
  (let ((mregexp regexp))
    (lambda (el)
      (string-match mregexp (plist-get el :item)))))

(defun org-expenses/filter-date (daterange)
  "Creates a filter function that keeps those entries that have a
date property within the given range.

DATERANGE can be an org date range, or only a partial date
construct, like '[2014-08]' which is then translated to a range
according to the most specific part. So '[2014-08]' expands to
'[2014-08-01]--[2014-08-31]'."
  (let ((range (org-expenses/expand-date-range daterange)))
    (lambda (el)
      (org-expenses/date-in-range-p
       range
       (plist-get el :date)))))



;; ---- searching


(defun org-expenses/make-search-filter (search &optional junct)
  "Take a search object and return a filter function to be
passed to `org-expenses/collect--props'.

SEARCH is a plist with properties, for example `:tags',
`:category', `:item' and `:date'. Those are looked up in the
variable `org-expenses/search-filters' that gives a function which
applied on the value of the key in SEARCH returns a filter
function. JUNCT may be one of :and, :or or :not (see
`org-expenses/collect--props' for more info)."
  (org-expenses/make--filter
   (cons (or junct :and)
         (org-expenses/plist-map search
          (lambda (k v)
            (-when-let (fun (plist-get org-expenses/search-filters k))
              (funcall fun v)))))))

(defun org-expenses/search-buffers (search files &optional maxsize)
  "Searches FILES for entries matching SEARCH. SEARCH is a p-list
  containing search criteria. For information on this see the doc
  string of `org-expenses/search'."
  (let ((filter (org-expenses/make-search-filter search))
        (size   0)
        (trunc  nil)
        (result nil))
    ;; search all files and concatenate results
    (-each files
      (lambda (file)
        (let ((items (org-expenses/collect--props (org-expenses/parse--file file)
                                             (if (bufferp file) (buffer-name file) file)
                                             filter)))
          (setq size (+ size (plist-get (car items) :size)))
          (setq result (append result (cdr items))))))
    ;; sort results by date desc
    (setq result (sort result (lambda (a b)
                                (string-lessp (plist-get b :date)
                                              (plist-get a :date)))))
    ;; apply maxsize
    (unless (null maxsize)
      (when (> size maxsize)
        (setq trunc t)
        (setq size maxsize)
        (setq result (-take maxsize result))))
    (cons (list :size size
                :truncated trunc
                :query search
                :files files)
          result)))

;; -------- sqlite cache

(defun org-expenses/sqlite-enabled ()
  "Check whether sqlite3 caching is enabled.

This is true, if `org-expenses/sqlite-db-file' is set to a file name."
  (and org-expenses/sqlite-db-file
       (stringp org-expenses/sqlite-db-file)))

(defun org-expenses/sqlite--cache-exists ()
  "Check whether the sqlite3 db file exists."
  (and (org-expenses/sqlite-enabled)
       (file-exists-p org-expenses/sqlite-db-file)))

(defun org-expenses/sqlite--convert-results (buf)
  "Convert results from a sqlite query into a plist.

The sqlite3 command must be invoked with option '-line'."
  (with-current-buffer buf
    (goto-char (point-min))
    (insert "'((")
    (while (< (point) (point-max))
      (if (looking-at "^$")
          (progn (insert ")")
                 (beginning-of-line 2)
                 (insert "("))
        (when (looking-at "[ \t]+\\|\n")
          (forward-whitespace 1))
        (if (looking-at "[a-zA-Z0-9\\-]+")
            (insert ":")
          (error (concat "Wrong format: " (buffer-string))))
        (search-forward "=" nil t)
        (backward-delete-char 1)
        (while (looking-at "[ \t]")
          (delete-char 1))
        (if (looking-at "[ \t]*$")
            (insert "nil"))
        (beginning-of-line 2)))
    (insert "))")
    (goto-char (point-min))
    (if (looking-at "'(())")
        nil
      (eval (read buf)))))

(defmacro org-expenses/with-sqlite-buffer (cmd file &rest body)
  "Execute sqlite3 CMD against database FILE.

If CMD returns successfully, BODY is evaluated with the current
buffer set to a temporary buffer that contains the output of
CMD. Additionally CMD is logged to a '*sqlite-log*' buffer and if
CMD failed, the output is logged, too. Otherwise output is not
logged, but this may be overriden by settings
`org-expenses/log-sqlite-output' to t."
  `(let ((db (or (when ,file
                   (expand-file-name ,file))
                 (when org-expenses/sqlite-db-file
                   (expand-file-name org-expenses/sqlite-db-file))
                 (user-error "No db file specified.")))
        (sqlite org-expenses/sqlite-cmd)
        (cmds (mapconcat 'identity (-list ,cmd) "; "))
        (log  (get-buffer-create "*sqlite-log*")))
     (with-current-buffer log
       (newline)
       (insert "> Execute: " cmds)
       (newline))
     (with-temp-buffer
       (let ((rc (call-process sqlite nil t nil "-line" db cmds))
             (out (current-buffer)))
         (with-current-buffer log
           (insert "  " (number-to-string rc) (if (eq rc 0) " success." " failed."))
           (newline)
           (when (or org-expenses/log-sqlite-output (/= 0 rc))
             (insert "--- output ---")
             (newline)
             (insert-buffer-substring out)
             (newline)
             (insert "--- end output ---")
             (newline)))
         (if (eq rc 0)
             (progn
               ,@body)
           (error "Error return from sqlite command: %d" rc))))))

(put 'org-expenses/with-sqlite-buffer 'lisp-indent-function 2)


(defun org-expenses/sqlite-exec (cmd &optional file)
  "Execute the sqlite3 CMD.

Return t on success, otherwise nil. FILE can be a database file,
or the default in `org-expenses/sqlite-db-file' is used."
  (org-expenses/with-sqlite-buffer cmd file t))

(defun org-expenses/sqlite-exec-lines (cmd &optional file)
  "Execute sqlite3 CMD and return the output lines in a list.

FILE can be a database file, or the default in
`org-expenses/sqlite-db-file' is used."
  (org-expenses/with-sqlite-buffer cmd file
    (split-string (buffer-string) "[\r\n]+" t)))

(defun org-expenses/sqlite-select (query &optional file)
  "Execute a sqlite3 query and return the results as a plist.

FILE can be a database file, or the default in
`org-expenses/sqlite-db-file' is used. The results are converted into
a plist where the key is the column name converted into a
keyword. Thus, it is necessary to have column names that make up
valid keywords (the colon is prepended and should not be part of
the column name)."
  (org-expenses/with-sqlite-buffer query file
    (org-expenses/sqlite--convert-results (current-buffer))))

(defun org-expenses/sqlite--table-p (tablename &optional file)
  "Check if TABLENAME is a table in database FILE.

FILE can be a database file, or the default in
`org-expenses/sqlite-db-file' is used."
  (when (org-expenses/sqlite-select (format "pragma table_info('%s')" tablename)
                               file)
    t))

(defun org-expenses/sqlite--column-p (table column &optional file)
  "Check if TABLE and COLUMN exists in database FILE.

FILE can be a database file, or the default in
`org-expenses/sqlite-db-file' is used."
  (let ((colname  (if (string-match "^\".*?\"$" column)
                      (substring column 1 (1- (length column)))
                    column))
        (info (org-expenses/sqlite-select (format "pragma table_info('%s')" table)
                                     file)))
    (car (-mapcat
          (lambda (col)
            (org-expenses/plist-map col (lambda (k v)
                                     (and (string= (substring (symbol-name k) 1) "name")
                                          (string= v colname))) t))
          info))))

(defun org-expenses/file-last-mod-sec (file)
  "Return the last modification time in seconds for FILE."
  (round (float-time (nth 5 (file-attributes file)))))

(defun org-expenses/sqlite--file-imported-p (file)
  "Check whether FILE is in the database in its current version."
  (let* ((lastmod   (org-expenses/file-last-mod-sec file))
         (lastmoddb (when (org-expenses/sqlite--table-p "files")
                      (plist-get
                       (car (org-expenses/sqlite-select
                             (format "select time from files where name = '%s'"
                                     file)))
                       :time))))
    (<= lastmod (or lastmoddb 0))))

(defun org-expenses/sqlite--make-column-name (key)
  "Convert KEY into a column name for sqlite3.

If KEY is a keyword, the colon is removed. The a string is
surrounded with quotation marks for proper escaping."
  (cond ((keywordp key) (format "\"%s\"" (substring (symbol-name key) 1)))
        ((stringp key) (format "\"%s\"" key))
        (t (error "Unknown type for making column"))))

(defun org-expenses/sqlite--make-value-string (obj)
  "Convert object into a value string for storing in sqlite3.

OBJ may be any lisp expression which is converted into its string
representaion using `prin1-to-string'. This string can then be
easily read and evaluated when converting query results. Single
quotation marks are doubled as to not conflict with sqlite3's
strings."
  (let ((repr (prin1-to-string obj)))
    (if (numberp obj)
        repr
      (concat "'"
              (replace-regexp-in-string "'"
                                        "''"
                                        repr
                                        nil
                                        t)
              "'"))))

(defun org-expenses/sqlite--collect-columns (items)
  "Collect all keys from the plists in ITEMS."
  (let ((result '()))
    (dolist (item (cdr items))
      (org-expenses/plist-map item
        (lambda (k v)
          (unless (or (null v) (plist-member result k))
            (setq result (plist-put result k v))))))
    result))

(defun org-expenses/sqlite--adjust-columns (table columns &optional dbfile)
  "Return sql statements to alter TABLE adding COLUMNS if they don't exist yet.

DBFILE is the database file, if not specified
`org-expenses/sqlite-db-file' is used. COLUMNS is a plist of
key-values that should be stored in the table TABLE. If a column
does not exist, its sqlite3 type is concluded (integer → integer,
number → numeric, others → text) and an 'alter table' statement
is created. These statements are returned in a list."
  (org-expenses/plist-map columns
                     (lambda (k v)
                       (let ((col (org-expenses/sqlite--make-column-name k)))
                         (unless (org-expenses/sqlite--column-p table col dbfile)
                           (format "alter table %s add column %s %s"
                                   table
                                   col
                                   (cond
                                    ((integerp v) "integer")
                                    ((numberp v) "numeric")
                                    (t "text"))))))))

(defun org-expenses/sqlite--make-insert (item &optional table)
  "Create an sql insert statements to insert ITEM into TABLE.

If TABLE is not specified, the default table is used."
  (let* ((columns   (-distinct (org-expenses/plist-kmap item 'identity)))
         (sqlfmt (concat "insert into "
                         (or table "expenses")
                         " ("
                         (mapconcat 'org-expenses/sqlite--make-column-name columns ", ")
                         ") values (%s)"))
         (vals (org-expenses/plist-map item
                                  (lambda (_k v)
                                    (org-expenses/sqlite--make-value-string v)))))
    (format sqlfmt (mapconcat 'identity vals ", "))))

(defun org-expenses/sqlite--import (file)
  "Parse FILE and insert all expense items into the database.

At first all current values for this file are deleted. Then all
expense items in FILE are inserted into the sqlite3 database."
  (org-expenses/sqlite-exec (format "delete from expenses where filename = '\"%s\"'"
                               file))
  (let* ((items (org-expenses/collect--props
                 (org-expenses/parse--file file)
                 file
                 'identity))
         (columns (org-expenses/sqlite--collect-columns items))
         (batchsize org-expenses/sqlite-import-batchsize))
    (message "Importing %d entries. Please hold on…"
             (plist-get (car items) :size))
    (org-expenses/sqlite-exec
     (mapconcat 'identity
                (org-expenses/sqlite--adjust-columns "expenses" columns)
                "; "))
    (let ((rest (-reduce-from
                 (lambda (acc item)
                   (if (< (length acc) batchsize)
                       (cons (org-expenses/sqlite--make-insert item) acc)
                     (org-expenses/sqlite-exec (mapconcat 'identity acc "; "))
                     (list (org-expenses/sqlite--make-insert item))))
                 '()
                 (cdr items))))
      (when rest
        (org-expenses/sqlite-exec (mapconcat 'identity rest "; "))))
    (org-expenses/sqlite-exec (format "delete from files where name = '%s';" file))
    (org-expenses/sqlite-exec (list (format
                                "insert into files (name, time) values ('%s', %d);"
                                file
                                (org-expenses/file-last-mod-sec file))))))

(defun org-expenses/sqlite--init-schema (&optional dbfile)
  "Creates the two tables in database DBFILE."
  (let ((expenses "create table expenses (filename text, item text, category text, tags text, date text)")
        (files "create table files (name text, time int)"))
  (unless (org-expenses/sqlite--table-p "expenses" dbfile)
    (org-expenses/sqlite-exec expenses dbfile))
  (unless (org-expenses/sqlite--table-p "files" dbfile)
    (org-expenses/sqlite-exec files dbfile))))

(defun org-expenses/sqlite-import-files (files)
  "Import FILES into database.
It first checks whether a files has changed since the last import
and will only import if necessary."
  (org-expenses/sqlite--init-schema)
  (dolist (file files)
    (unless (org-expenses/sqlite--file-imported-p file)
      (org-expenses/sqlite--import file))))


(defun org-expenses/sqlite--search-expand-values (colname values &optional opfun valfun)
  "Create a combination of COLNAME = VALUES conditions.

COLNAME is a column name and VALUES a list of values. This
returns a list of conditions like COLNAME OPFUN value, where
OPFUN determines the comparator based on the first character of
value. By convention a + means inclusion and - exclusion. VALFUN
is applied to each value to return a proper value string for
sqlite3.

Example:
   (org-expenses/sqlite--search-expand-values \"tags\" '(\"a\" \"-b\" \"+c\"))
   ;; (\"tags = 'a'\" \"tags <> 'b'\" \"tags = 'c'\")"
  (let ((opfun0 (or opfun (lambda (char) (if (eq ?- char) " <> " " = "))))
        (valfun0 (or valfun (lambda (val) (concat "'" val "'")))))
    (-map (lambda (lst)
            (concat colname
                    (funcall opfun0 (car lst))
                    (funcall valfun0 (nth 1 lst))))
          (-map 'org-expenses/dissect--char-prefix values))))


(defun org-expenses/sqlite--search-combine (lst &optional op)
  "Create a string from LST by combining each element via OP.

LST must be a list of strings. Then for '(\"a\" \"b\" \"c\")
return \")a OP b OP c)\"."
  (when lst
    (concat "(" (mapconcat 'identity lst (or op " and ")) ")")))


(defun org-expenses/sqlite--search-tags (tags)
  "Create a sql condition to match all tag names in TAGS."
  (org-expenses/sqlite--search-combine
   (org-expenses/sqlite--search-expand-values
    "tags"
    (-list tags)
    (lambda (char) (if (eq ?- char) " not like " " like "))
    (lambda (val) (concat "'%" val "%'")))))

(defun org-expenses/sqlite--search-category (categories)
  "Create a sql condition to match all category names in CATEGORIES."
  (org-expenses/sqlite--search-combine
   (org-expenses/sqlite--search-expand-values
    "category"
    (-list categories)
    (lambda (char) (if (eq ?- char) " <> " " = "))
    (lambda (val) (concat "'\"" val "\"'")))))

(defun org-expenses/sqlite--search-item (item)
  "Create a sql condition to match ITEM."
  (concat "item like '%" item "%'"))

(defun org-expenses/sqlite--search-date (date)
  "Create a sql condition to match date range DATE."
  (let* ((range (org-expenses/date-range-tuple (org-expenses/expand-date-range date)))
         (beg   (substring (car range) 0 11)) ;; removes last bracket
         (end   (cdr range)))
    (concat "(date >= '\"" beg "' and date <= '\"" end "\"')")))

(defun org-expenses/sqlite--search-to-where (search)
  "Converts the search plist SEARCH into a sql expression.

Return an empty string if SEARCH is nil or doesn't contain known
keys."
  (let ((where (org-expenses/sqlite--search-combine
                (org-expenses/plist-map
                 search
                 (lambda (key values) (cond
                                  ((eq key :tags)
                                   (org-expenses/sqlite--search-tags values))
                                  ((eq key :category)
                                   (org-expenses/sqlite--search-category values))
                                  ((eq key :item)
                                   (org-expenses/sqlite--search-item values))
                                  ((eq key :date)
                                   (org-expenses/sqlite--search-date values))))))))
    (if where
        (concat "where " where)
      "")))



(defun org-expenses/sqlite-search (search files &optional maxsize)
  "Search sqlite cache for items matching SEARCH.

FILES are the files to search. They are imported if
necessary. This function is analog to `org-expenses/search-buffers'
but uses sqlite3 to cache values. MAXSIZE can be used to restrict
the result size."
  (org-expenses/sqlite-import-files files)
  (let* ((query (org-expenses/sqlite--search-to-where search))
         (count (plist-get (car (org-expenses/sqlite-select
                                 (concat "select count(*) as size from expenses "
                                         query)))
                           :size))
         (result (org-expenses/sqlite-select
                  (concat "select * from expenses "
                          query " order by date desc"
                          (if maxsize (format " limit %d" maxsize) ""))))
         (trunc nil)
         (size  count))
    (when (and maxsize
               (> count maxsize))
      (setq trunc t)
      (setq size maxsize))
    (cons (list :files files
                :query search
                :size size
                :truncated trunc)
          result)))


(defun org-expenses/search (search &optional files)
  "Search through your expense entries.

If FILES is non-nil, search these files or buffers instead of
files in `org-expenses/files'. FILES must be either a list of files or
a list of buffers.

Return those that match the given SEARCH criteria. SEARCH is a
property-list with the following optional properties:

- :tags A single string or a list of strings specifying tag
  names. Tag names starting with a minus '-' means that those
  entries are excluded.

- :category A single string or a list of strings denoting the
  categories to match. As with tags you can exclude categories
  by prefixing them with a minus '-'.

- :item A string that is matched against the item name

- :date A date range string. This can be a full org date range,
  like '[2014-07-10]--[2014-08-10]' or a partial date like
  '2014-08' where the range is created from the most specific
  part of the date string. Brackets are not required around the
  dates.

This function returns a list of property-lists each but the first
representing one expense item. The first element is a property
list with some meta information about the results:

- :size the length of the result list

- :query the SEARCH argument

- :files the FILES argument

An expense item must match all given search criterias. If
`org-expenses/sqlite-db-file' is set, sqlite3 is used to cache
values."
  (if files
      (org-expenses/search-buffers search
                              (-list files)
                              org-expenses/max-results)
    (let ((files (org-expenses/file-list)))
      (if org-expenses/sqlite-db-file
          (org-expenses/sqlite-search search files org-expenses/max-results)
        (org-expenses/search-buffers search files org-expenses/max-results)))))


(defun org-expenses/retrieve-tag-names (&optional files)
  "Return a sorted list of tag names used on expense items in FILES."
  (let ((items (cdr (org-expenses/search nil files))))
    (sort (-distinct (-mapcat (lambda (el) (plist-get el :tags)) items))
          'string-lessp)))

(defun org-expenses/retrieve-category-names (&optional files)
  "Return a sorted list of category names used on expense items in FILES."
  (let ((items (cdr (org-expenses/search nil files))))
    (sort (-distinct (-map (lambda (el) (plist-get el :category)) items))
          'string-lessp)))

;;;; grouping

(defun org-expenses/group-results (results fn)
  "Group the RESULTS of `org-expenses/search' by FN.

The meta element of RESULTS (the first element) is preserved.

Separate cdr of RESULTS into an alist whose keys are FN applied
to the elements of RESULTS.  Keys are compared by `equal'."
  (cons (car results)
        (-group-by fn (cdr results))))

(defun org-expenses/group-by-property (key &optional default use-equal)
  "Use with `org-expenses/group-results' to group results based on some property value."
  (lambda (el)
    (or (if use-equal
            (lax-plist-get el key)
          (plist-get el key))
        (or default "<unknown>"))))

(defun org-expenses/group-by-category (el)
  "Use with `org-expenses/group-results' to group results based on category."
  (or (plist-get el :category) "uncategorized"))

(defun org-expenses/group-by-month (el)
"Use with `org-expenses/group-results' to group results based on month."
  (let* ((date (plist-get el :date))
        (dateparts (org-expenses/parse-date-parts date)))
    (if date
        (format "%d/%02d" (car dateparts) (cadr dateparts))
      "no date")))

(defun org-expenses/group-by-year (el)
  "Use with `org-expenses/group-results' to group results based on year."
  (let* ((date (plist-get el :date))
        (dateparts (org-expenses/parse-date-parts date)))
    (if date
        (format "%04d" (car dateparts))
      "no date")))


;;;; summing

(defun org-expenses/summarize-results (results)
"Summarize RESULTS by calculating sum, min, max, count and avg."
  (let* ((items (cdr results))
         (compare-fun (lambda (a b) (> (org-expenses/as-number a)
                                  (org-expenses/as-number b))))
         (currs (-distinct (-mapcat 'org-expenses/filter--currencies
                                    items))))
    (-mapcat
     (lambda (curr)
       (let* ((values (delq nil
                            (-map (lambda (el)
                                    (plist-get el curr))
                                  items)))
              (sum (-reduce 'org-expenses/n+ values))
              (cnt (length values))
              (min (-min-by compare-fun values))
              (max (-max-by compare-fun values)))
         (unless (null values)
           (list curr
                 (list :sum sum
                       :max (or max min)
                       :min min
                       :count cnt
                       :avg (/ sum cnt))))))
     currs)))

(defun org-expenses/summarize-grouped-result (grouped-result)
  "Apply `org-expenses/summarize-results' to each item list in GROUPED-RESULT.

GROUPED-RESULT is an alist as returned from
`org-expenses/group-results'. Return a new alist, with keys as in
GROUPED-RESULT alist but associated to a summary plist of its
items."
  (let ((items (cdr grouped-result)))
    (-map (lambda (pair)
            (cons (car pair)
                  (org-expenses/summarize-results (cons 'dummy (cdr pair)))))
          items)))


;;;;;; tables

(defun org-expenses/make--summary-table (summary)
  "Make a list of rows from SUMMARY.

SUMMARY is expected to be retrieved via `org-expenses/summarize-results'."
  (let ((currs (org-expenses/sort-keywords (org-expenses/plist-kmap summary 'identity)))
        (head (list '("Curr." "sum" "max" "min" "count" "avg") 'hline)))
    (append
     head
     (-map (lambda (curr)
            (let ((values (plist-get summary curr)))
              (list (org-expenses/as-string curr)
                    (org-expenses/format-price (plist-get values :sum))
                    (org-expenses/format-price (plist-get values :max))
                    (org-expenses/format-price (plist-get values :min))
                    (number-to-string (plist-get values :count))
                    (org-expenses/format-price (plist-get values :avg)))))
          currs))))

(defun org-expenses/make--group-summary-table (summary)
  "Make a list of rows from SUMMARY.

SUMMARY is expected to be retrieved via
`org-expenses/summarize-grouped-result', which returns an alist where
the keys are some discriminator value and the values are summary
plists."
  (let ((head (list '("" "sum" "max" "min" "count" "avg") 'hline)))
    (append
     head
     (-mapcat
      (lambda (group)
        (let* ((name (car group))
               (sums (cdr group))
               (currs (org-expenses/sort-keywords (org-expenses/plist-kmap sums 'identity))))
          (-map
           (lambda (curr)
             (let ((values (plist-get sums curr)))
               (list (concat name " " (org-expenses/as-string curr))
                     (org-expenses/format-price (plist-get values :sum))
                     (org-expenses/format-price (plist-get values :max))
                     (org-expenses/format-price (plist-get values :min))
                     (number-to-string (plist-get values :count))
                     (org-expenses/format-price (plist-get values :avg)))))
           currs)))
      summary))))

(defun org-expenses/summary-table (results &optional groupfn)
  "Create a list of rows of a summary of RESULTS.

If GROUPFN is given, RESULTS is first grouped by this function
and summaries are calculated for each sub-results."
  (if (functionp groupfn)
      (org-expenses/make--group-summary-table
       (org-expenses/summarize-grouped-result
        (org-expenses/group-results results groupfn)))
    (org-expenses/make--summary-table
     (org-expenses/summarize-results results))))


(defun org-expenses/result-table (results columns)
  "Make a list of rows each representing an item in RESULTS.

COLUMNS is a list of keys used to retrieve values for each
column. Currency columns are added by this function and should
not be specified."
  (let* ((summary (org-expenses/summarize-results results))
         (currs (org-expenses/sort-keywords (org-expenses/plist-kmap summary 'identity)))
         (cols (append columns currs))
         (head (list (-map 'org-expenses/as-string cols) 'hline)))
    (append head
            (-map (lambda (el)
                    (-map (lambda (key)
                            (if (member key currs)
                                (org-expenses/format-price (plist-get el key))
                              (or (plist-get el key) "")))
                          cols))
                  (cdr results)))))

(defun org-expenses/insert--table (table)
  "Insert TABLE at point."
  (insert (orgtbl-to-orgtbl table nil)))

;;;; interactive things

(defun org-expenses/insert-summary-table (search &optional group-fn)
  "Search for items using SEARCH and insert the summary table at point.

If GROUP-FN is given, it is used to group the results first."
  (interactive "xSearch: \naGroup-Fn: ")
  (let* ((result (org-expenses/search search))
         (gresult (when (functionp group-fn)
                    (org-expenses/group-results result group-fn)))
         (summary (if gresult
                      (org-expenses/summarize-grouped-result gresult)
                    (org-expenses/summarize-results result))))
    (org-expenses/insert--table
     (if gresult
         (org-expenses/make--group-summary-table summary)
       (org-expenses/make--summary-table summary)))))

(defun org-expenses/insert-item-table (search)
  "Search for items using SEARCH and insert them as an org table."
  (interactive "xSearch: ")
  (let ((result (org-expenses/search search)))
    (org-expenses/insert--table
     (org-expenses/result-table result org-expenses/item-columns))))


;;;;; overview mode

(defvar org-expenses/expense-view-mode-hook nil
  "List of functions to call when entering expense-view mode.")

(defvar org-expenses/expense-view-mode-map nil
  "Keymap for expense-view-mode")

(defun org-expenses/expense-view-mode ()
  "Mode for expense overview buffer.
\\{org-expenses/expense-view-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (org-mode)
  (setq major-mode 'org-expenses/expense-view-mode)
  (setq mode-name "Expenses")
  (set (make-local-variable 'items-table-line) nil)
  (set (make-local-variable 'for-buffer) nil)
  (set (make-local-variable 'search) (make-ring 30))
  (set (make-local-variable 'follow-entries) nil)
  (set (make-local-variable 'follow-window) nil)
  (org-expenses/expense-view--set-search
   (list :date
         (format "%d-%02d"
                 (nth 2 (calendar-current-date))
                 (car (calendar-current-date)))))
  (set (make-local-variable 'results) nil)
  (use-local-map org-expenses/expense-view-mode-map)
  (run-hooks 'org-expenses/expense-view-mode-hook))

(defmacro org-expenses/in-view-mode (&rest body)
  `(if (not (equal 'org-expenses/expense-view-mode major-mode))
       (user-error "This only works in expense-view-mode!")
     ,@body))

(defun org-expenses/expense-view--set-search (obj)
  (org-expenses/in-view-mode
   (ring-insert search obj)))

(defun org-expenses/expense-view--get-search ()
  (org-expenses/in-view-mode
   (ring-ref search 0)))

(defun org-expenses/expense-view--pop-search ()
  (org-expenses/in-view-mode
   (ring-remove search 0)))

(defun org-expenses/expense-view--change-search (&rest plist)
  "Change the current search object by applying PLIST.
Values in search are overriden by those in PLIST. Keys in PLIST
with a nil value cause the key-value pair to be removed from
search."
  (let* ((current (-copy (org-expenses/expense-view--get-search))))
    (org-expenses/plist-map plist
                       (lambda (k v)
                         (setq current
                               (if (null v)
                                   (org-expenses/plist-remove current k)
                                 (plist-put current k v)))
                         nil))
    (org-expenses/expense-view--set-search current)))


(defun org-expenses/expense-view-quit-window (kill)
  (interactive "P")
  "Kills the expense view window."
  (org-expenses/in-view-mode
   (when follow-entries
     (org-expenses/expense-view-toggle-follow-entries))
   (quit-window kill)))

(defun org-expenses/find--date-range (results)
  "Find the earliest and most current date in RESULTS."
  (-reduce-from (lambda (a b)
                  (let ((date (plist-get b :date))
                        (min (car a))
                        (max (cdr a)))
                    (cons (if (org-time>= date min) min date)
                          (if (org-time<= max date) date max))))
                '("[9999-12-31]" . "[1971-01-01]")
                (cdr results)))

(defun org-expenses/expense-view-update-view (&optional arg)
  "Update the expense-view buffer.
With one prefix ARG all caches are cleared first."
  (interactive "P")
  (when (and (equal arg '(4)) (org-expenses/sqlite--cache-exists))
    (delete-file org-expenses/sqlite-db-file))
  (setq results (org-expenses/search (org-expenses/expense-view--get-search) for-buffer))
  (erase-buffer)
  (let ((range (org-expenses/find--date-range results)))
    (insert (format "#+TITLE: Expenses %s - %s\n#+STARTUP: showeverything"
                    (car range)
                    (cdr range)))
    (newline 2)
    (insert "Search: ~")
    (insert (if (org-expenses/expense-view--get-search)
                (prin1-to-string (org-expenses/expense-view--get-search))
              "All") "~")
    (insert (format "\nShowing *%d* items"
                    (plist-get (car results) :size)))
    (when (plist-get (car results) :truncated)
      (newline)
      (insert "_The list is truncated._"))
    (newline 2)
    (insert "* Overview")
    (newline)
    (-each org-expenses/overview-tables
      (lambda (el)
        (let ((name (car el))
              (tablefun (cdr el))
              (items results))
          (insert name)
          (newline)
          (insert (with-temp-buffer
                    (org-expenses/insert--table (funcall tablefun items))
                    (buffer-string)))
          (newline 2))))
    (insert "* Items")
    (newline)
    (setq items-table-line (line-number-at-pos))
    (org-expenses/insert--table
     (org-expenses/result-table results org-expenses/item-columns)))
  (goto-char (point-min)))

(defun org-expenses/expense-view (&optional arg)
  "Show expense view of all items in `org-expenses/files'.

The view is set to current month initially. With one prefix ARG,
clear all caches first (if applicable). With two prefix ARG,
display the expense view for current buffer only. The current
buffer must be in org mode for this to work."
  (interactive "P")
  (let ((currentbuf (equal arg '(16)))
        (cbuf (current-buffer))
        (buf (get-buffer-create "*expenses*")))
    (when (and currentbuf (not (equal major-mode 'org-mode)))
      (user-error "Current buffer not in org-mode."))
    (set-buffer buf)
    (unless (equal 'expense-view-mode major-mode)
      (org-expenses/expense-view-mode)
      (setq for-buffer (if currentbuf cbuf nil))
      (org-expenses/expense-view-update-view arg))
    (pop-to-buffer buf)))

(defun org-expenses/expense-view-move-date (n)
  "Moves the current date range by N.
N is added to the most specific date part currently set."
  (interactive "nMove date by: ")
  (let ((date (plist-get (org-expenses/expense-view--get-search) :date)))
    (unless (null date)
      (let ((next (org-expenses/date-range-move date n)))
        (unless (or (null next) (equal next date))
          (org-expenses/expense-view--change-search :date next)
          (org-expenses/expense-view-update-view))))))

(defun org-expenses/expense-view--table-item ()
  "Return the item from the result corresponding to current row in item table."
  (unless (< (line-number-at-pos) items-table-line)
    (let ((nr (- (line-number-at-pos) items-table-line 1)))
      (unless (< nr 0)
        (nth nr results)))))

(defun org-expenses/expense-view-next-date (arg)
  "Move the date range by ARG."
  (interactive "p")
  (org-expenses/expense-view-move-date arg))

(defun org-expenses/expense-view-prev-date (arg)
  "Move the date range by -ARG."
  (interactive "p")
  (org-expenses/expense-view-move-date (* -1 arg)))

(defun org-expenses/expense-view--show-item-tags ()
  "When on a line in the items table, show the tags of current item."
  (let ((item (org-expenses/expense-view--table-item)))
    (unless (null item)
      (message "Tags: %s"
               (mapconcat 'identity (plist-get item :tags) ", ")))))

(defun org-expenses/expense-view-move-next ()
  "Move point to the next table."
  (interactive)
  (org-expenses/in-view-mode
   (let ((p (search-forward-regexp org-table-dataline-regexp nil t))
         (beg (org-table-begin)))
     (unless (null p)
       (when (= (+ 2 beg) p)
         (org-expenses/expense-view-move-next))
       (when follow-entries
         (org-expenses/expense-view-jump-to-entry))
       (org-expenses/expense-view--show-item-tags)))))

(defun org-expenses/expense-view-move-prev ()
  "Move point to the previous table."
  (interactive)
  (org-expenses/in-view-mode
   (forward-line -1)
   (unless (and (save-excursion
                  (move-beginning-of-line nil)
                  (looking-at org-table-dataline-regexp))
                (/= (+ 2 (org-table-begin)) (point)))
     (org-expenses/expense-view-move-prev))
   (when follow-entries
     (org-expenses/expense-view-jump-to-entry))
   (org-expenses/expense-view--show-item-tags)))

(defun org-expenses/expense-view-toggle-follow-entries ()
  "Toggle to follow entries in item table."
  (interactive)
  (org-expenses/in-view-mode
   (setq follow-entries (not follow-entries))
   (when follow-entries
     (org-expenses/expense-view-jump-to-entry))
   (when (and (not follow-entries) follow-window)
     (quit-window t follow-window))))

(defun org-expenses/expense-view-jump-to-entry ()
  "When on a line in items table, open scroll to the item in its file."
  (interactive)
  (org-expenses/in-view-mode
   (let* ((item (org-expenses/expense-view--table-item))
          (pos  (plist-get item :begin))
          (file (plist-get item :filename))
          (win  (selected-window))
          (otherwin nil))
     (unless (null item)
       (find-file-other-window file)
       (show-all)
       (goto-char pos)
       (setq otherwin (selected-window))
       (select-window win)
       (setq follow-window otherwin)))))

(defun org-expenses/expense-view-toggle-table ()
  "Move between first and items table."
  (interactive)
  (org-expenses/in-view-mode
   (if (> (line-number-at-pos) items-table-line)
       (progn
         (goto-char (point-min))
         (org-expenses/expense-view-move-next))
     (goto-char (point-min))
     (forward-line (1- items-table-line))
     (org-expenses/expense-view-move-next))))


(defun org-expenses/expense-view-focus-item (arg)
  "When on an item, search for all such items.
With prefix arg, replace search object otherwise only set the
item value."
  (interactive "P")
  (let ((field (s-trim (string-make-unibyte (org-table-get-field 1)))))
    (if arg
        (org-expenses/expense-view--set-search (list :item field))
      (org-expenses/expense-view--change-search :item field))
    (org-expenses/expense-view-update-view)))

(defun org-expenses/expense-view-last-search ()
  "Use previous search and update view."
  (interactive)
  (org-expenses/expense-view--pop-search)
  (org-expenses/expense-view-update-view))

(defun org-expenses/expense-view-goto-this-month ()
  "Show items in the current month."
  (interactive)
  (org-expenses/expense-view--change-search
   :date
   (format "%d-%02d"
           (nth 2 (calendar-current-date))
           (car (calendar-current-date))))
  (org-expenses/expense-view-update-view))

(defun org-expenses/expense-view-search-item (str)
  "Ask for a string and search for all such items."
  (interactive "sItem: ")
  (org-expenses/expense-view--change-search
   :item
   (if (or (null str) (s-equals-p "" str)) nil str))
  (org-expenses/expense-view-update-view))

(defun org-expenses/expense-view-search-date (str)
  "Ask for a date and search for items within this range."
  (interactive "sDate range: ")
  (org-expenses/expense-view--change-search
   :date
   (if (or (null str) (s-equals-p "" str)) nil str))
  (org-expenses/expense-view-update-view))

(defun org-expenses/completing-read (prompt table)
  (if (fboundp 'ido-completing-read)
      (ido-completing-read prompt table)
    (completing-read prompt table)))

(defun org-expenses/expense-view-search-add-tag (tagname)
  "Ask for a tag and add it to the current search."
  (interactive
   (let* ((taglist (org-expenses/retrieve-tag-names))
          (all (append taglist (-map (lambda (s) (concat "-" s)) taglist))))
     (list (org-expenses/completing-read "Tags: " all))))
  (let ((current (-list (plist-get (org-expenses/expense-view--get-search) :tags))))
    (org-expenses/expense-view--change-search :tags (-distinct (cons tagname current)))
    (org-expenses/expense-view-update-view)))

(defun org-expenses/expense-view-search-remove-tag (tagname)
  "Ask for a tag and remove it from the current search."
  (interactive
   (let* ((all (-list (plist-get (org-expenses/expense-view--get-search) :tags))))
     (if (= 1 (length all))
          all
       (list (ido-completing-read "Tags: " all)))))
  (let ((current (-list (plist-get (org-expenses/expense-view--get-search) :tags))))
    (org-expenses/expense-view--change-search :tags (remove tagname current))
    (org-expenses/expense-view-update-view)))

(defun org-expenses/expense-view-search-add-category (name)
  "Ask for a category name and add it to the current search."
  (interactive
   (let* ((categories (org-expenses/retrieve-category-names))
          (all (append categories (-map (lambda (s) (concat "-" s)) categories))))
     (list (org-expenses/completing-read "Category: " all))))
  (let ((current (-list (plist-get (org-expenses/expense-view--get-search) :category))))
    (org-expenses/expense-view--change-search :category (-distinct (cons name current)))
    (org-expenses/expense-view-update-view)))

(defun org-expenses/expense-view-search-remove-category (name)
  "Ask for a category name and remove it from the current search."
  (interactive
   (let* ((all (-list (plist-get (org-expenses/expense-view--get-search) :category))))
     (if (= 1 (length all))
          all
       (list (org-expenses/completing-read "Category: " all)))))
  (let ((current (-list (plist-get (org-expenses/expense-view--get-search) :category))))
    (org-expenses/expense-view--change-search :category (remove name current))
    (org-expenses/expense-view-update-view)))

(defun org-expenses/expense-view-widen-date ()
  "Extend the date range currently in use."
  (interactive)
  (let* ((date (plist-get (org-expenses/expense-view--get-search) :date))
         (next (org-expenses/abbrev-date-range (org-expenses/date-range-extend date 1))))
    (org-expenses/expense-view--change-search :date next)
    (org-expenses/expense-view-update-view)))

(defun org-expenses/expense-view-narrow-date ()
  "Narrow the date range currently in use."
  (interactive)
  (let* ((date (plist-get (org-expenses/expense-view--get-search) :date))
         (next (org-expenses/abbrev-date-range (org-expenses/date-range-extend date -1))))
    (unless (null next)
      (org-expenses/expense-view--change-search :date next)
      (org-expenses/expense-view-update-view))))

(if org-expenses/expense-view-mode-map
    nil
  (setq org-expenses/expense-view-mode-map (make-sparse-keymap))
  (suppress-keymap org-expenses/expense-view-mode-map)
  (define-key org-expenses/expense-view-mode-map "q" 'org-expenses/expense-view-quit-window)
  (define-key org-expenses/expense-view-mode-map "f" 'org-expenses/expense-view-next-date)
  (define-key org-expenses/expense-view-mode-map "b" 'org-expenses/expense-view-prev-date)
  (define-key org-expenses/expense-view-mode-map "+" 'org-expenses/expense-view-widen-date)
  (define-key org-expenses/expense-view-mode-map "-" 'org-expenses/expense-view-narrow-date)
  (define-key org-expenses/expense-view-mode-map "." 'org-expenses/expense-view-goto-this-month)
  (define-key org-expenses/expense-view-mode-map "/" 'org-expenses/expense-view-search-item)
  (define-key org-expenses/expense-view-mode-map "d" 'org-expenses/expense-view-search-date)
  (define-key org-expenses/expense-view-mode-map "t" 'org-expenses/expense-view-search-add-tag)
  (define-key org-expenses/expense-view-mode-map "T" 'org-expenses/expense-view-search-remove-tag)
  (define-key org-expenses/expense-view-mode-map "c" 'org-expenses/expense-view-search-add-category)
  (define-key org-expenses/expense-view-mode-map "C" 'org-expenses/expense-view-search-remove-category)
  (define-key org-expenses/expense-view-mode-map "n" 'org-expenses/expense-view-move-next)
  (define-key org-expenses/expense-view-mode-map (kbd "<down>") 'org-expenses/expense-view-move-next)
  (define-key org-expenses/expense-view-mode-map (kbd "\C-n") 'org-expenses/expense-view-move-next)
  (define-key org-expenses/expense-view-mode-map "p" 'org-expenses/expense-view-move-prev)
  (define-key org-expenses/expense-view-mode-map (kbd "<up>") 'org-expenses/expense-view-move-prev)
  (define-key org-expenses/expense-view-mode-map (kbd "\C-p") 'org-expenses/expense-view-move-prev)
  (define-key org-expenses/expense-view-mode-map "v" 'org-expenses/expense-view-focus-item)
  (define-key org-expenses/expense-view-mode-map "l" 'org-expenses/expense-view-last-search)
  (define-key org-expenses/expense-view-mode-map (kbd "RET") 'org-expenses/expense-view-jump-to-entry)
  (define-key org-expenses/expense-view-mode-map "F" 'org-expenses/expense-view-toggle-follow-entries)
  (define-key org-expenses/expense-view-mode-map (kbd "\C-t") 'org-expenses/expense-view-toggle-table)
  (define-key org-expenses/expense-view-mode-map "g" 'org-expenses/expense-view-update-view))


(provide 'org-expenses)

;;; org-expenses.el ends here
