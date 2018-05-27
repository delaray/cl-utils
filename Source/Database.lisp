(in-package :Utilities)



;;;-----------------------------------------------------------------------------
;;; PGDB Variables
;;;-----------------------------------------------------------------------------

(eval-when (eval load compile)
  (defparameter *PG-PASSWORD*  "")
  (defparameter *PG-DB* "DFDB")
  (defparameter *PG-DB-NAME* "DFDB")
  )

;;;-----------------------------------------------------------------------------
;;; INITIALIZE-PGDB
;;;-----------------------------------------------------------------------------

;;; This initializes the connection to PostGres.

(defun INITIALIZE-PGDB (&key (db *pg-db-name*))
  (unless (pgdb-connected-p)
    (pomo::connect-toplevel db "postgres" *pg-password* "localhost"
			    :port 5432)))

;;;-----------------------------------------------------------------------------
;;; PGRID-CONNECTED-P
;;;-----------------------------------------------------------------------------

(defun PGDB-CONNECTED-P ()
  (and pomo::*database*
       (pomo::connected-p pomo::*database*)))

;;;-----------------------------------------------------------------------------
;;; QUERYING POSTGRES
;;;-----------------------------------------------------------------------------

(defmacro WITH-PGDB ((&optional (db-name *pg-db-name*)) &body body)
    `(pomo::with-connection (list ,db-name
                             "postgres" ""
                             "localhost" :port 5432
                             :pooled-p t)
       ,@body))


;;;-----------------------------------------------------------------------------

(defun QUERY-PGDB (query-string &rest args)
  (with-pgdb ()
    (pomo::query (apply 'format nil query-string args))))

;;;-----------------------------------------------------------------------------
;;; UTILS
;;;-----------------------------------------------------------------------------

(defun PGDB-CLEAR ()
  (pomo::query "drop VIEW as_provider_item"))

;;;-----------------------------------------------------------------------------

(defun DC-EXTEND ()
  (pomo::query
   "CREATE or replace VIEW as_provider_item AS
          SELECT vendor_item_id_strip,
                 vendor_item_id as vencatnum,
                 v.name as vendor_name,
                 m.name as mfr_name,
                 a.manufacturer_item_id as mfrcatnum,
                 item_description, qoe, uom, cost, 
                 trademark, composition, size
          FROM all_source_items as a,
               nuvia_vendors as v,
               nuvia_vendors as m
          WHERE v.id = a.nuvia_vendor_id
                and m.id = a.manufacturer_id"))

;;;-----------------------------------------------------------------------------
;;; INDEX-ALLSOURCE
;;-----------------------------------------------------------------------------

(defun INDEX-ALLSOURCE ()
  (with-pgdb ()
    (pomo:query "CREATE INDEX allsource_items_by_vencatnum
               ON all_source_items
               USING btree (vendor_item_id_strip);")))

;;;-----------------------------------------------------------------------------

(defun QUOTE-TICK (u)
  (let* ((tick (code-char 39)))
    (if (find tick u)
        (with-output-to-string (e)
          (loop for c across u
              when (eql c tick)
              do (princ tick e)
              do (princ c e)))
      u)))

;;;-----------------------------------------------------------------------------
;;; Test Functions
;;;-----------------------------------------------------------------------------

#+IGNORE
(defun DC-TEST-1 (&key (vcn "M732317"))
  (pprint (find-allsource-items-with-vencatnum vcn)))

;;;-----------------------------------------------------------------------------

#+IGNORE
(defun DC-TEST-2 (&key (vcn "M732317"))
  (funcall
   (write-pg-query-results (find-allsource-items-with-vencatnum vcn)
     "/home/kenny/test-client.txt")))

;;;-----------------------------------------------------------------------------
;;;  PROVIDER-ITEM
;;;-----------------------------------------------------------------------------

;;; This holds one PG Result Record.

#+IGNORE
(defstruct (provider-item
            (:type vector)
            (:conc-name pi-))
  vendor-name
  vencatnum
  vencatnum-stripped
  manufacturer-name 
  mfrcatnum
  description
  noun 
  type 
  trademark 
  composition 
  size 
  primary-size 
  secondary-size 
  outer-diameter 
  liquid-volume 
  volume 
  weight 
  age  
  shape 
  usage 
  color 
  ergonomics-location
  gender 
  flavor 
  fragrance 
  sterility 
  latexity 
  disposability
  hazardous-material 
  qoe 
  uom 
  cost 
  quantity 
  packaging 
  unspsc 
  hcpcs 
  charge-master 
  properties 
  other
  source-id
  source-type)

#+IGNORE
(defstruct (provider-po
               (:type vector)
               (:conc-name ppo-))
  vendor-name
  vencatnum
  vencatnum-stripped
  manufacturer-name 
  mfrcatnum
  description
  qoe 
  uom 
  cost 
  quantity 
  unit-cost
  extended-cost
  po-num
  po-date
  )


;;; These are the headers that are potentially mapped.

(defparameter *provider-item-slots*
  '(vendor-name
    vencatnum
    vencatnum-stripped
    manufacturer-name 
    mfrcatnum
    description
    noun 
    type 
    trademark 
    composition 
    size 
    primary-size 
    secondary-size 
    outer-diameter 
    liquid-volume 
    volume 
    weight 
    age  
    shape 
    usage 
    color 
    ergonomics-location
    gender 
    flavor 
    fragrance 
    sterility 
    latexity 
    disposability
    hazardous-material 
    qoe 
    uom 
    cost 
    quantity 
    packaging 
    unspsc 
    hcpcs 
    charge-master 
    properties 
    other
    source-id
    source-type))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
