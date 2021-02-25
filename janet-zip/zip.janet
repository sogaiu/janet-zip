# XXX: differs from clojure's behavior
#      e.g. (butlast [:a]) would yield nil(?!) in clojure
(defn butlast
  "Get all but the last of an indexed value."
  [indexed]
  (if (empty? indexed)
    nil
    (let [new-indexed (array/slice indexed 0 -2)]
      (if (tuple? indexed)
        (tuple ;new-indexed)
        new-indexed))))

(comment

  (butlast [:a :b :c])
  # => [:a :b]

  (butlast [:a])
  # => []

  (butlast [])
  # => nil

  (butlast @[:a :b :c])
  # => @[:a :b]

  (butlast @[:a])
  # => @[]

  (butlast @[])
  # => nil

  )

(defn butfirst
  "Get all but the first of an indexed value."
  [indexed]
  (if (empty? indexed)
    nil
    (let [new-indexed (array/slice indexed 1 -1)]
      (if (tuple? indexed)
        (tuple ;new-indexed)
        new-indexed))))

(comment

  (butfirst [:a :b :c])
  # => [:b :c]

  (butfirst [:a :b])
  # => [:b]

  (butfirst [:a])
  # => []

  (butfirst [])
  # => nil

  (butfirst @[:a :b :c])
  # => @[:b :c]

  (butfirst @[:a :b])
  # => @[:b]

  (butfirst @[:a])
  # => @[]

  (butfirst @[])
  # => nil

  )

(defn tuple/push
  "Adds x to the end of the tuple. If tuple is nil the value is
  returned as the only value in a tuple."
  [tup x & xs]
  (if tup
    [;tup x ;xs]
    [x ;xs]))

(comment

  (tuple/push [:a :b] :c)
  # => [:a :b :c]

  (tuple/push nil :a)
  # => [:a]

  (tuple/push [:a] :b :c)
  # => [:a :b :c]

  (tuple/push [] :a)
  # => [:a]

  )

(defn car-cdr-all
  "Splits a sequence into car, cdr, and the original sequence."
  [xs]
  (if (or (nil? xs) (empty? xs))
    [nil nil nil]
    [(first xs) (butfirst xs) xs]))

(comment

  (car-cdr-all [:a])
  # => [:a [] [:a]]

  (car-cdr-all [:a :b])
  # => [:a [:b] [:a :b]]

  (car-cdr-all @[:a])
  # => [:a @[] @[:a]]

  (car-cdr-all @[:a :b])
  # => [:a @[:b] @[:a :b]]

  (car-cdr-all [])
  # => [nil nil nil]

  # XXX: is this what we want?
  (car-cdr-all nil)
  # => [nil nil nil]

  )

(defn to-entries
  "Converts a dictionary into a tuple of key-value tuples."
  [val]
  (if (dictionary? val)
    (partition 2 (kvs val))
    val))

(comment

  (to-entries {:a 1 :b 2})
  # => @[[:a 1] [:b 2]]

  (to-entries {:a 1})
  # => @[[:a 1]]

  (to-entries {})
  # => @[]

  (to-entries @{:a 1 :b 2})
  # => @[[:a 1] [:b 2]]

  (to-entries @{:a 1})
  # => @[[:a 1]]

  (to-entries @{})
  # => @[]

  # XXX: leaving non-dictionaries alone and passing through...
  #      is this desirable over erroring?
  (to-entries [:a :b :c])
  # => [:a :b :c]

  )

(defn zipper
  "Creates a new zipper structure composed of three elements:

    `[root state fns]`

  - `root` is the passed in node.
  - `state` is information about where the node is situated in the tree.
  - `fns` are a struct of the three functions passed into this.

  :branch? - fn that determines if a node is a branch (has children)
             or a leaf.

  :children - fn that finds the child nodes for the given branch.

  :make-node - takes a node + children and returns a new branch node
               with the same.
  "
  [root &keys {:branch? branch?
               :children children
               :make-node make-node}]
  [root
   nil
   {:branch? branch?
    :children children
    :make-node make-node}])

(defn zip
  "Returns a zipper for nested sequences (tuple/array/table/struct),
  given a root sequence."
  [sequence]
  (zipper
    sequence
    :branch? |(or (dictionary? $) (indexed? $))
    :children to-entries
    :make-node (fn [p xs] xs)))

(defn node
  "Returns the node at the given location."
  [location]
  (location 0))

(comment

  (def a-zip
    (zip [:a :b [:x :y]]))

  (node a-zip)
  # => [:a :b [:x :y]]

  )

(defn state
  "Returns the state for the given location."
  [location]
  (location 1))

(comment

  (def b-zip
    (zip [:a :b [:x :y]]))

  (state b-zip)
  # => nil

  )

(defn fns
  "Returns the fns for the given location."
  [location]
  (location 2))

(comment

  (def c-zip
    (zip [:a :b [:x :y]]))

  (= to-entries
    ((fns c-zip) :children))
  # => true

  )

(defn branch?
  "Returns true if the node at the given location is a branch.
  Returns false otherwise."
  [location]
  (((fns location) :branch?) (node location)))

(comment

  (def d-zip
    (zip [:a :b [:x :y]]))

  (branch? d-zip)
  # => true

  )

(defn children
  "Gets the children of a branch node at the given location.

  Raises an error if the given location is a leaf."
  [location]
  (if (branch? location)
    (((fns location) :children) (node location))
    (error "called children on a leaf node")))

(comment

  (def e-zip
    (zip [:a :b [:x :y]]))

  (children e-zip)
  # => [:a :b [:x :y]]

  )

(defn make-node
  "Convenience function for calling the :make-node fn from the fns at
  the given location."
  [location node children]
  (((fns location) :make-node) node children))

(comment

  (def f-zip
    (zip [:a :b [:x :y]]))

  (make-node f-zip
    [:a :b] [:x :y])
  # => [:x :y]

  )

(defn path
  "Returns the sequence of nodes that lead to this location."
  [location]
  (when-let [st (state location)]
    (st :pnodes)))

(comment

  (def g-zip
    (zip [:a :b [:x :y]]))

  (path g-zip)
  # => nil

  )

(defn down
  "Moves down the tree, returning the leftmost child location of the given
  location, or nil if there are no children."
  [location]
  (when (branch? location)
    (let [[node st _] location
          [c cnext cs] (car-cdr-all (children location))]
      (when cs
        [c
         @{:l []
           :pnodes (if st (tuple/push (st :pnodes) node) [node])
           :ppath st
           :r cnext}
         (fns location)]
        ))))

(comment

  (def h-zip
    (zip [:a :b [:x :y]]))

  (node (down h-zip))
  # => :a

  (path (down h-zip))
  # => [[:a :b [:x :y]]]

  )

(defn right
  "Returns the location of the sibling directly to the right of the
  node at the given location, or nil if there is no sibling to the right."
  [location]
  (let [[node st fns] location
        {:l l :r r} (or st @{})
        [r rnext rs] (car-cdr-all r)]
    (when (and st rs)
      [r
       (merge st {:l (tuple/push l node) :r rnext})
       fns])))

(comment

  (def i-zip
    (zip [:a :b [:x :y]]))

  (right (down i-zip))

  (node (down (right (right (down i-zip)))))
  # => :x

  (path (down (right (right (down i-zip)))))
  # => [[:a :b [:x :y]] [:x :y]]

  )

(defn left
  "Returns the location of the sibling directly to the left of the
  node at the given location, or nil if there is no sibling to the left."
  [location]
  (let [[node st fns] location
        {:l l :r r} (or st @{})]
    (when (and path
               (indexed? l)
               (not (empty? l)))
      [(last l)
       (merge st {:l (butlast l) :r [node ;r]})
       fns])))

(comment

  (def j-zip
    (zip [:a :b [:x :y]]))

  (deep=
    (left (right (down j-zip)))
    (down j-zip))
  # => true

  )

(defn lefts
  "Returns the sequence of siblings to the left of this location."
  [location]
  (if-let [st (state location)
           l (st :l)]
    l
    []))

(comment

  (def k-zip
    (zip [:a :b [:x :y]]))

  (lefts (right (right (down k-zip))))
  # => [:a :b]

  (lefts (right (down (right (right (down k-zip))))))
  # => [:x]

  )

(defn rights
  "Returns the sequence of siblings to the right of this location."
  [location]
  (when-let [st (state location)]
    (st :r)))

(comment

  (def l-zip
    (zip [:a :b [:x :y]]))

  (rights (down l-zip))
  # => [:b [:x :y]]

  )

(defn up
  "Moves up the tree, returning the parent location of the given
  location, or nil if at the root."
  [location]
  (let [[node st fns] location
        {:l l :pnodes pnodes :ppath ppath :r r :changed? changed?}
        (or st @{})]
    (when pnodes
      (let [pnode (last pnodes)]
        (if changed?
          [(make-node location pnode [;l node ;r])
           (and ppath (merge ppath {:changed? true}))
           fns]
          [pnode
           ppath
           fns])))))

(comment

  (def m-zip
    (zip [:a :b [:x :y]]))

  (deep=
    (up (down m-zip))
    m-zip)
  # => true

  (deep=
    (up (left (right (down m-zip))))
    m-zip)
  # => true

  )

(defn end?
  "Returns true if location represents the end of a depth-first walk."
  [location]
  (= :end (state location)))

(defn next
  "Moves to the next location in the hierarchy, depth-first.  When
  reaching the end, returns a distinguished location detectable
  via end?.  If already at the end, stays there."
  [location]
  (defn recur
    [loc]
    (if (up loc)
      (or (right (up loc))
          (recur (up loc)))
      [(node loc) :end]))
  (if (end? location)
    location
    (or (and (branch? location) (down location))
        (right location)
        (recur location))))

(comment

  (def n-zip
    (zip [:a :b [:x :y]]))

  (node (next n-zip))
  # => :a

  (node (next (next n-zip)))
  # => :b

  (node (next (next (next n-zip))))
  # => [:x :y]

  (node (next (next (next (next n-zip)))))
  # => :x

  (node (next (next (next (next (next n-zip))))))
  # => :y

  (node (next (next (next (next (next (next n-zip)))))))
  # => [:a :b [:x :y]]

  (node (next (next (next (next (next (next (next n-zip))))))))
  # => [:a :b [:x :y]]

  (end? (next (next (next (next (next (next n-zip)))))))
  # => true

  )

(defn root
  "Moves all the way up the tree for the given location and returns the
  node at the root location."
  [location]
  (if (end? location)
    (node location)
    (if-let [p (up location)]
      (root p)
      (node location))))

(comment

  (def o-zip
    (zip [:a :b [:x :y]]))

  (deep=
    (-> o-zip
        down
        right
        right
        down
        root)
    (node o-zip))
  # => true

  )

(defn rightmost
  "Returns the location of the sibling furthest to the right of the node
  at the given location, or the current node if there are none to the
  right."
  [location]
  (let [[node st fns] location
        {:l l :r r} (or st @{})]
    (when (and st r)
      [(last r)
       (merge st {:l (tuple/push l node ;(butlast r)) :r nil})
       fns])))

(comment

  (def p-zip
    (zip [:a :b [:x :y]]))

  (node (rightmost (down p-zip)))
  # => [:x :y]

  (node (rightmost (down (right (right (down p-zip))))))
  # => :y

  )

(defn leftmost
  "Returns the location of the sibling furthest to the left of the node
  at the given location, or the current node if there are none to the
  left."
  [location]
  (let [[node st fns] location
        {:l l :r r} (or st @{})]
    (if (and st
             (indexed? l)
             (not (empty? l)))
      [(first l)
       (merge st {:l [] :r [;(butfirst l) node ;r]})
       fns]
      location)))

(comment

  (def q-zip
    (zip [:a :b [:x :y]]))

  (node (leftmost (right (down q-zip))))
  # => :a

  (node (leftmost (down (right (right (down q-zip))))))
  # => :x

  )

(defn insert-left
  "Inserts the item as the left sibling of the node at this location,
  without moving."
  [location item]
  (let [[node st fns] location
        {:l l} (or st @{})]
    (if st
      [node
       (merge st {:l (tuple/push l item) :changed? true})
       fns]
      (error "Insert at top"))))

(comment

  (def r-zip
    (zip [:a :b [:x :y]]))

  (root (insert-left (down r-zip) :z))
  # => [:z :a :b [:x :y]]

  (try
    (insert-left r-zip :e)
    ([e] e))
  # => "Insert at top"

  )

(defn insert-right
  "Inserts the item as the right sibling of the node at this location,
  without moving."
  [location item]
  (let [[node st fns] location
        {:r r} (or st @{})]
    (if st
      [node
       (merge st {:r [item ;r] :changed? true})
       fns]
      (error "Insert at top"))))

(comment

  (def s-zip
    (zip [:a :b [:x :y]]))

  (root (insert-right (down s-zip) :z))
  # => [:a :z :b [:x :y]]

  (try
    (insert-left s-zip :e)
    ([e] e))
  # => "Insert at top"

  )

(defn replace
  "Replaces the node at this location, without moving."
  [location node]
  (let [[_ st fns] location
        st (or st @{})]
    [node
     (merge st {:changed? true})
     fns]))

(comment

  (def t-zip
    (zip [:a :b [:x :y]]))

  (root (replace (down t-zip) :w))
  # => [:w :b [:x :y]]

  (root (replace (down (right (right (down t-zip)))) :w))
  # => [:a :b [:w :y]]

  )

(defn edit
  "Replaces the node at this location with the value of (f node args)."
  [location f & args]
  (replace location
           (apply f (node location) args)))

(comment

  (def u-zip
    (zip [1 2 [8 9]]))

  (root (edit (down u-zip) inc))
  # => [2 2 [8 9]]

  (root (edit (right (down (right (right (down u-zip))))) dec))
  # => [1 2 [8 8]]

  (root (edit (down (right (right (down u-zip))))
              + 2))
  # => [1 2 [10 9]]

  )

(defn insert-child
  "Inserts the item as the leftmost child of the node at this location,
  without moving."
  [location item]
  (replace location
           (make-node location
                      (node location)
                      [item ;(children location)])))

(comment

  (def v-zip
    (zip [:a :b [:x :y]]))

  (root (insert-child v-zip :c))
  # => [:c :a :b [:x :y]]

  (root (insert-child (right (right (down v-zip))) :c))
  # => [:a :b [:c :x :y]]

)

(defn append-child
  "Appends the item as the rightmost child of the node at this location,
  without moving."
  [location item]
  (replace location
           (make-node location
                      (node location)
                      [;(children location) item])))

(comment

  (def w-zip
    (zip [:a :b [:x :y]]))

  (root (append-child w-zip :c))
  # => [:a :b [:x :y] :c]

  (root (append-child (right (right (down v-zip))) :c))
  # => [:a :b [:x :y :c]]

)

(defn prev
  "Moves to the previous location in the hierarchy, depth-first.
  If already at the root, returns nil."
  [location]
  (defn recur
    [loc]
    (if-let [child (and (branch? loc) (down loc))]
      (recur (rightmost child))
      loc))
  (if-let [left-location (left location)]
    (recur left-location)
    (up location)))

(comment

  (def x-zip
    (zip [:a :b [:x :y]]))

  (node (prev (right (down x-zip))))
  # => :a

  (node (prev (right (right (down x-zip)))))
  # => :b

  (node (prev (down (right (right (down x-zip))))))
  # => [:x :y]

  (node (prev (prev (down (right (right (down x-zip)))))))
  # => :b

  )

(defn remove
  [location]
  "Removes the node at location, returning the location that would have
  preceded it in a depth-first walk."
  [location]
  (let [[node st fns] location
        {:l l :pnodes pnodes :ppath ppath :r r} (or st @{})]
    (defn recur
      [loc]
      (if-let [child (and (branch? loc) (down loc))]
        (recur (rightmost child))
        loc))
    (if st
      (if (pos? (length l))
        (recur [(last l)
                (merge st {:l (butlast l)
                           :changed? true})
                fns])
        [(make-node location (last pnodes) r)
         (and ppath (merge ppath {:changed? true}))
         fns])
      (error "Remove at top"))))

(comment

  (def y-zip
    (zip [:a :b [:x :y]]))

  (node (remove (right (down y-zip))))
  # => :a

  (root (remove (right (down y-zip))))
  # => [:a [:x :y]]

  (root (remove (right (right (down y-zip)))))
  # => [:a :b]

  (try
    (remove y-zip)
    ([e] e))
  # => "Remove at top"

  )
