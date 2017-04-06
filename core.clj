(ns myclojurehomework2.core)

;;color cgrid

;You have a set of N colors, and are given not more than N "color bars," 
;All color bars are the same length, and each bar may have duplicated or omitted colors. 
;The colors on a color bar can be changed by rotating them one step to the right, end around: 
;For example, Red-Yellow-Green-Blue can be rotated to Blue-Red-Yellow-Green.

;Your task is to write a function solve that takes as a parameter a list of lists of colors, 
;each sublist representing one color bar. Colors may be represented by integers, strings or symbols (such as :red). 
;The goal is to rotate each color bar until no column contains duplicate colors. 
;For example, if there are five color bars, each column should have five different colors in it.

;Write a separate function to print the solution. The solve function should not do any printing.

;;Function which creates a list of all integers in a given range.

(def new-range [[\R \Y \G \B], 
                [\R \B \V \O],
                [\R \G \G \B],
                [\O \R \O \V]])

;(defn process [lst1 lst2] 
;  (if (not(=(nth new-range n) (nth new-range n)))
;    (rotate [lst1 lst]
;    true)) )

;This method rotates a given lst, which is a row of the grid
(defn rotate [llst]
  (let [n (count llst)]
  (concat (drop (dec n) llst) (take (dec n) llst)) ))

;this basically takes in a list llst and returns the rotated lst
(defn get-rotated-row [llst]
  (let [a (dec (count llst) ) lst (nth llst a)]
  (rotate lst)
  ))

;this basically compares the compatibilty of 2 lists and returns a boolean
(defn compability [lst1 lst2]
  (let [zipped-list (zip (lst1 lst2))] ;we zip the 2 lists and then check each first and second component
   (not-any? (fn[x](=(fst x)(snd x)))
    zipped-list)) )

;takes in a row1 and a row2 of lst (row2 being next row after row)
(defn functionrecurs [row1 row2 grid]
  (cond (> ((inc row2) (count grid))) ;test that row we're comparing against exists 
    false ;base case
    (compatibility row1 row2) ;calls compatibility on row1 and row2
    (functionrecurs row1 (inc row2) grid) ;if compatability returns true (thus no column equal), 
    ;then increment row2 by 1 and recurs on comparison with row1
    :else ;very last thing that can occur in this loop 
    (functionrecurs row1 (inc row2) (createnewlst grid)) ;if it fails then we call the create-newlist function
    ;which in essence will create a new grid iwth the rotate row and then recurs on that
  ) )

;this calls the functionrecurs by incrementing to the next row
(defn recurs2 [row1 row2 grid]
  (functionrecurs (inc row1) (inc row2) grid))

;this creates a new grid
(defn createnewlst [grid]
  (let [firstrows (take (row1) grid)])
  (let [new-row-a (get-rotated-row [row1])])
  (let [lastrows (take-last (row1+1) )])  
  (concat [firstrows] [new-row1] [lastrows]) )

(defn solve [grid]
  (let [n (count grid)]
    (functionrecurs row1 row2 grid)
    (cond (> ((inc row1) (n) ) ) 
          (recurs2 row1 row2 grid))))

(defn print [grid]
  (solve grid))


                 ;(defn newlst [lst]
                 ;  (update-in [
                 ; 
                 ; 
                 ;(defn new-range [x]
                 ;  (= (first (first x ) ) (first (second x)) )
                 ;
                 ;
                 ;
                 ;
                 ; (process ret (take n-1)
                 ;          (drop n-1)ret))
                 ;
                 ; (defn rotate (nth new-range n))
                 ;
                 ;
                 ;
                 ; (nth (nth new-range 3) 3)
                 ;
                 ; (defn solve
                 ;   (= (nth (nth new-range 3) 3) (nth (nth new-range 3) 3)
                 ;
                 ; (defn solve (new-range)
                 ;   new-solution)
                 ;
                 ; (defn solve [llst]
                 ;   llst)
                 ;
                 ; (print (solve new-range))
                 ;