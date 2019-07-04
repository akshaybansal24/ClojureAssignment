(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(def customer_data_from_file (slurp "cust.txt"))
(def customer_data_from_file_split (clojure.string/split-lines customer_data_from_file))
(def cust(sort customer_data_from_file_split))
(def total_no_of_customer (count cust))
(defstruct customer_struct :id :name :address :phone)
(def customer_data (vector))

(defn sort_customer[customer1 customer2]
  (if (< (:id customer1) (:id customer2))
    true
    false
  )
)

(defn display_customer_table[]
  (def customer_data_sorted (sort-by :id customer_data))
  
  (println)
  (def index(atom 0))
  (while (< @index total_no_of_customer)
    (do
      (def record_customer(nth customer_data_sorted @index))
      (print (get record_customer :id))
      (print ": [\""(get record_customer :name)"\"")
      (print " \""(get record_customer :address)"\"")
      (print " \""(get record_customer :phone)"\"]\n")
      (swap! index inc)
    )  
  )
)

(defn process_customer_data[]
  (def index (atom 0))
  (def customer_data (vector))
  (while (< @index total_no_of_customer)
    (do
      (def line_data (nth customer_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def customer_record (struct customer_struct (parse-int (get line_data_split 0)) (get line_data_split 1) (get line_data_split 2) (get line_data_split 3)))
      (def customer_data (conj customer_data customer_record))
      (swap! index inc)
    )
  )
  (display_customer_table)
)

(def product_data_from_file (slurp "prod.txt"))
(def product_data_from_file_split (clojure.string/split-lines product_data_from_file))
(def prod(sort product_data_from_file_split))
(def total_no_of_product (count prod))
(defstruct product_struct :id :description :cost)
(def product_data (vector))

(defn sort_product[product1 product2]
  (if (< (:id product1) (:id product2))
    true
    false
  )
)

(defn display_product_table[]
  (def product_data_sorted (sort-by :id product_data))
  (println)
  (def index(atom 0))
  (while (< @index total_no_of_product)
    (do
      (def record_product(nth product_data_sorted @index))
      (print (get record_product :id))
      (print ": [\""(get record_product :description)"\"")
      (print " \""(get record_product :cost)"\"]\n")
      (swap! index inc)
    )  
  )
)

(defn process_product_data[]
  (def index (atom 0))
  (def product_data (vector))
  (while (< @index total_no_of_product)
    (do
      (def line_data (nth product_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def product_record (struct product_struct (parse-int (get line_data_split 0)) (get line_data_split 1) (get line_data_split 2)))
      (def product_data (conj product_data product_record))
      (swap! index inc)
    )
  )
  (display_product_table)
)

(def sales_data_from_file (slurp "sales.txt"))
(def sales_data_from_file_split (clojure.string/split-lines sales_data_from_file))
(def sls(sort sales_data_from_file_split))
(def total_no_of_sales (count sls))
(defstruct sales_struct :id :custID :prodID :count :customer_name :product_name)
(def sales_data (vector))

(defn sort_sales[sales1 sales2]
  (if (< (:id sales1) (:id sales2))
    true
    false
  )
)

(defn display_sales_table[]
  (def sales_data_sorted (sort-by :id sales_data))
  (println)
  (def index(atom 0))
  (while (< @index total_no_of_sales)
    (do
      (def record_sales(nth sales_data_sorted @index))
      (print (get record_sales :id))
      (print ": [\""(get record_sales :customer_name)"\"")
      (print " \""(get record_sales :product_name)"\"")
      (print " \""(get record_sales :count)"\"]\n")
      (swap! index inc)
    )  
  )
)

(defn process_sales_data[]
  (def customer_name_vector(vector))
  (def customer_id_vector(vector))
  (def product_name_vector(vector))
  (def product_id_vector (vector))

  (def index (atom 0))
  (def product_data (vector))
  (while (< @index total_no_of_product)
    (do
      (def line_data (nth product_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def product_id_vector (conj product_id_vector (get line_data_split 0)))
      (def product_name_vector (conj product_name_vector (get line_data_split 1)))
      (swap! index inc)
    )
  )
  
  (def index (atom 0))
  (def customer_data (vector))
  (while (< @index total_no_of_customer)
    (do
      (def line_data (nth customer_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def customer_id_vector (conj customer_id_vector (get line_data_split 0)))
      (def customer_name_vector (conj customer_name_vector (get line_data_split 1)))
      (swap! index inc)
    )
  )
  
  (def index (atom 0))
  (def sales_data (vector))
  (while (< @index total_no_of_sales)
    (do
      (def line_data (nth sales_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def customer_index (.indexOf customer_id_vector (get line_data_split 1)))
      (def product_index (.indexOf product_id_vector (get line_data_split 2)))
      (def customer_name (get customer_name_vector customer_index))
      (def product_name (get product_name_vector product_index))
      (println customer_name)
      (println product_name)
      (def sales_record (struct sales_struct (parse-int (get line_data_split 0)) (get line_data_split 1) (get line_data_split 2) (get line_data_split 3) customer_name product_name))
      (def sales_data (conj sales_data sales_record))
      (swap! index inc)
    )
  )
  (display_sales_table)
)


(defn total_value_of_purchase_for_customer[]
  (println "\nEnter the customer name : ")
  (def customer_name_entered (read-line))
  (def customer_name_vector(vector))
  (def customer_id_vector(vector))
  (def product_name_vector(vector))
  (def product_id_vector (vector))
  (def product_price_vector (vector))
  (def index (atom 0))
  (def product_data (vector))
  (while (< @index total_no_of_product)
    (do
      (def line_data (nth product_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def product_id_vector (conj product_id_vector (get line_data_split 0)))
      (def product_name_vector (conj product_name_vector (get line_data_split 1)))
      (def product_price_vector (conj product_price_vector (get line_data_split 2)))
      (swap! index inc)
    )
  )
  (def index (atom 0))
  (def customer_data (vector))
  (while (< @index total_no_of_customer)
    (do
      (def line_data (nth customer_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def customer_id_vector (conj customer_id_vector (get line_data_split 0)))
      (def customer_name_vector (conj customer_name_vector (get line_data_split 1)))
      (swap! index inc)
    )
  )
  (def total_sum 0)
  (def index (atom 0))
  (def sales_data (vector))
  (while (< @index total_no_of_sales)
    (do
      (def line_data (nth sales_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def customer_index (.indexOf customer_id_vector (get line_data_split 1)))
      (def product_index (.indexOf product_id_vector (get line_data_split 2)))
      (def customer_name (get customer_name_vector customer_index))
      (if (= customer_name customer_name_entered)
        (do
          (def product_price (read-string(get product_price_vector product_index)))
          (def product_count (read-string(get line_data_split 3)))
          (def total_sum (+ (* product_price product_count) total_sum) )
          
        )
      )
      (swap! index inc)
    )
  )
  (println)
  (println customer_name_entered" : $"total_sum)
 )

(defn total_sales_of_product[]
  (println "\nEnter the product name : ")
  (def product_name_entered (read-line))
  (def product_name_vector(vector))
  (def product_id_vector (vector))
  (def index (atom 0))
  (while (< @index total_no_of_product)
    (do
      (def line_data (nth product_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def product_id_vector (conj product_id_vector (get line_data_split 0)))
      (def product_name_vector (conj product_name_vector (get line_data_split 1)))
      (swap! index inc)
    )
  )
  (def total_sum 0)
  (def index (atom 0))
  (while (< @index total_no_of_sales)
    (do
      (def line_data (nth sales_data_from_file_split @index))
      (def line_data_split (clojure.string/split line_data #"\|"))
      (def product_index (.indexOf product_id_vector (get line_data_split 2)))
      (def product_name (get product_name_vector product_index))
      (if (= product_name product_name_entered)
        (do
          (def product_count (read-string(get line_data_split 3)))
          (def total_sum (+ (* 1 product_count) total_sum) )
          
        )
      )
      (swap! index inc)
    )
  )
  (println)
  (println product_name_entered" : "total_sum)
 )


(def loop_var (atom 1))
(def choice (atom 0))

(defn main_menu[]
  (println "*** Sales Menu ***")
  (println "------------------")
  (println)
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit")
  (println)
  (println "Enter an option?")
)

(main_menu)

(while( = @loop_var 1)
  (do
    (def flag (atom 0))
    (reset! choice (read-line))
    
    ;;Display Customer Table
    (if(= "1" @choice)
      (do
        (reset! flag 1)
        (process_customer_data)
        (println)
        (main_menu)
      )
    )
    
    ;;Display Product Table
    (if(= "2" @choice)
      (do
        (reset! flag 1)
        (process_product_data)
        (println)
        (main_menu)
      )
    )
    
    ;;Display Sales Table
    (if(= "3" @choice)
      (do
        (reset! flag 1)
        (process_sales_data)
        (println)
        (main_menu)
      )
    )
    
    ;;Total Sales for Customer
    (if(= "4" @choice)
      (do
        (reset! flag 1)
        (total_value_of_purchase_for_customer)
        (println)
        (main_menu)
      )
    )
    
    ;;Total Product for Customer
    (if(= "5" @choice)
      (do
        (reset! flag 1)
        (total_sales_of_product)
        (println)
        (main_menu)
      )
    )
    
    ;;Exit
    (if(= "6" @choice)
      (do
        (reset! flag 1)
        (reset! loop_var 0)
        (println "Good Bye!")
      )
    )
    
    ;;Default
    (if (= @flag 0)
      (println "You have entered wrong option. Please choose correct option from menu: ")
    )
  )
)
