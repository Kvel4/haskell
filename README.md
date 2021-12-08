
<main><h1 id="homework-0">Homework #0</h1>
<h2 id="task-1">Task 1</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T1</code> and define the following type in it:</p>
<pre><code>data a &lt;-&gt; b = Iso (a -&gt; b) (b -&gt; a)

flipIso :: (a &lt;-&gt; b) -&gt; (b &lt;-&gt; a)
flipIso (Iso f g) = Iso g f

runIso :: (a &lt;-&gt; b) -&gt; (a -&gt; b)
runIso (Iso f _) = f</code></pre></li>
<li><p>Implement the following functions and isomorphisms:</p>
<pre><code>distrib :: Either a (b, c) -&gt; (Either a b, Either a c)
assocPair :: (a, (b, c)) &lt;-&gt; ((a, b), c)
assocEither :: Either a (Either b c) &lt;-&gt; Either (Either a b) c</code></pre></li>
</ol>
<h2 id="task-2">Task 2</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T2</code> and define the following type in it:</p>
<pre><code>type Not a = a -&gt; Void</code></pre></li>
<li><p>Implement the following functions and isomorphisms:</p>
<pre><code>doubleNeg :: a -&gt; Not (Not a)
reduceTripleNeg :: Not (Not (Not a)) -&gt; Not a</code></pre></li>
</ol>
<h2 id="task-3">Task 3</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T3</code> and define the following combinators in it:</p>
<pre><code>s :: (a -&gt; b -&gt; c) -&gt; (a -&gt; b) -&gt; (a -&gt; c)
s f g x = f x (g x)

k :: a -&gt; b -&gt; a
k x y = x</code></pre></li>
<li><p>Using <em>only those combinators</em> and function application (i.e. no lambdas, pattern matching, and so on) define the following additional combinators:</p>
<pre><code>i :: a -&gt; a
compose :: (b -&gt; c) -&gt; (a -&gt; b) -&gt; (a -&gt; c)
contract :: (a -&gt; a -&gt; b) -&gt; (a -&gt; b)
permute :: (a -&gt; b -&gt; c) -&gt; (b -&gt; a -&gt; c)</code></pre>
<p>For example:</p>
<pre><code>i x = x         -- No (parameters on the LHS disallowed)
i = \x -&gt; x     -- No (lambdas disallowed)
i = Prelude.id  -- No (only use s and k)
i = s k k       -- OK
i = (s k) k     -- OK (parentheses for grouping allowed)</code></pre></li>
</ol>
<h2 id="task-4">Task 4</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T4</code>.</p></li>
<li><p>Using the <code>fix</code> combinator from the <code>Data.Function</code> module define the following functions:</p>
<pre><code>repeat&#39; :: a -&gt; [a]             -- behaves like Data.List.repeat
map&#39; :: (a -&gt; b) -&gt; [a] -&gt; [b]  -- behaves like Data.List.map
fib :: Natural -&gt; Natural       -- computes the n-th Fibonacci number
fac :: Natural -&gt; Natural       -- computes the factorial</code></pre>
<p>Do not use explicit recursion. For example:</p>
<pre><code>repeat&#39; = Data.List.repeat     -- No (obviously)
repeat&#39; x = x : repeat&#39; x      -- No (explicit recursion disallowed)
repeat&#39; x = fix (x:)           -- OK</code></pre></li>
</ol>
<h2 id="task-5">Task 5</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T5</code> and define the following type in it:</p>
<pre><code>type Nat a = (a -&gt; a) -&gt; a -&gt; a</code></pre></li>
<li><p>Implement the following functions:</p>
<pre><code>nz :: Nat a
ns :: Nat a -&gt; Nat a

nplus, nmult :: Nat a -&gt; Nat a -&gt; Nat a

nFromNatural :: Natural -&gt; Nat a
nToNum :: Num a =&gt; Nat a -&gt; a</code></pre></li>
<li><p>The following equations must hold:</p>
<pre><code>nToNum nz       ==  0
nToNum (ns x)   ==  1 + nToNum x

nToNum (nplus a b)   ==   nToNum a + nToNum b
nToNum (nmult a b)   ==   nToNum a * nToNum b</code></pre></li>
</ol>
<h2 id="task-6">Task 6</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T6</code> and define the following values in it:</p>
<pre><code>a = distrib (Left (&quot;AB&quot; ++ &quot;CD&quot; ++ &quot;EF&quot;))     -- distrib from HW0.T1
b = map isSpace &quot;Hello, World&quot;
c = if 1 &gt; 0 || error &quot;X&quot; then &quot;Y&quot; else &quot;Z&quot;</code></pre></li>
<li><p>Determine the WHNF (weak head normal form) of these values:</p>
<pre><code>a_whnf = ...
b_whnf = ...
c_whnf = ...</code></pre></li>
</ol>
<h1 id="homework-1">Homework #1</h1>
<h2 id="task-1-1">Task 1</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T1</code> and define the following data type in it:</p>
<pre><code>data Day = Monday | Tuesday | ... | Sunday</code></pre>
<p>(Obviously, fill in the <code>...</code> with the rest of the week days).</p>
<p>Do not derive <code>Enum</code> for <code>Day</code>, as the derived <code>toEnum</code> is partial:</p>
<pre><code>ghci&gt; toEnum 42 :: Day
*** Exception: toEnum{Day}: tag (42) is outside of enumeration&#39;s range (0,6)</code></pre></li>
<li><p>Implement the following functions:</p>
<pre><code>-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -&gt; Day

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -&gt; Day -&gt; Day

-- | Checks if the day is on the weekend.
isWeekend :: Day -&gt; Bool

-- | Computes the number of days until Friday.
daysToParty :: Day -&gt; Natural</code></pre>
<p>In <code>daysToParty</code>, if it is already <code>Friday</code>, the party can start immediately, we don’t have to wait for the next week (i.e. return <code>0</code> rather than <code>7</code>).</p>
<p><small>Good job if you spotted that this task is a perfect fit for modular arithmetic, but that is not the point of the exercise. The functions must be implemented by operating on <code>Day</code> values directly, without conversion to a numeric representation. </small></p></li>
</ol>
<h2 id="task-2-1">Task 2</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T2</code> and define the following data type in it:</p>
<pre><code>data N = Z | S N</code></pre></li>
<li><p>Implement the following functions:</p>
<pre><code>nplus :: N -&gt; N -&gt; N        -- addition
nmult :: N -&gt; N -&gt; N        -- multiplication
nsub :: N -&gt; N -&gt; Maybe N   -- subtraction     (Nothing if result is negative)
ncmp :: N -&gt; N -&gt; Ordering  -- comparison      (Do not derive Ord)</code></pre>
<p>The operations must be implemented without using built-in numbers (<code>Int</code>, <code>Integer</code>, <code>Natural</code>, and such).</p></li>
<li><p>Implement the following functions:</p>
<pre><code>nFromNatural :: Natural -&gt; N
nToNum :: Num a =&gt; N -&gt; a</code></pre></li>
<li><p>(Advanced) Implement the following functions:</p>
<pre><code>nEven, nOdd :: N -&gt; Bool    -- parity checking
ndiv :: N -&gt; N -&gt; N         -- integer division
nmod :: N -&gt; N -&gt; N         -- modulo operation</code></pre>
<p>The operations must be implemented without using built-in numbers.</p>
<p><small>In <code>ndiv</code> and <code>nmod</code>, the behavior in case of division by zero is not specified (you can throw an exception, go into an inifnite loop, or delete all files on the computer).</small></p></li>
</ol>
<h2 id="task-3-1">Task 3</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T3</code> and define the following data type in it:</p>
<pre><code>data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)</code></pre>
<p>The <code>Meta</code> field must store additional information about the subtree that can be accessed in constant time, for example its size. You can use <code>Int</code>, <code>(Int, Int)</code>, or a custom data structure:</p>
<pre><code>type Meta = Int         -- OK
data Meta = M Int Int   -- OK</code></pre>
<p>Functions operating on this tree must maintain the following invariants:</p>
<ol type="1">
<li><strong>Sorted</strong>: The elements in the left subtree are less than the head element of a branch, and the elements in the right subtree are greater.</li>
<li><strong>Unique</strong>: There are no duplicate elements in the tree (follows from <strong>Sorted</strong>).</li>
<li><strong>CachedSize</strong>: The size of the tree is cached in the <code>Meta</code> field for constant-time access.</li>
<li>(Advanced) <strong>Balanced</strong>: The tree is balanced according to one of the following strategies:
<ul>
<li><strong>SizeBalanced</strong>: For any given <code>Branch _ l _ r</code>, the ratio between the size of <code>l</code> and the size of <code>r</code> never exceeds <code>3</code>.</li>
<li><strong>HeightBalanced</strong>: For any given <code>Branch _ l _ r</code>, the difference between the height of <code>l</code> and the height of <code>r</code> never exceeds <code>1</code>.</li>
</ul></li>
</ol>
<p>These invariants enable efficient processing of the tree.</p></li>
<li><p>Implement the following functions:</p>
<pre><code>-- | Size of the tree, O(1).
tsize :: Tree a -&gt; Int

-- | Depth of the tree.
tdepth :: Tree a -&gt; Int

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a =&gt; a -&gt; Tree a -&gt; Bool

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a =&gt; a -&gt; Tree a -&gt; Tree a

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a =&gt; [a] -&gt; Tree a</code></pre>
<p>Tip 1: in order to maintain the <strong>CachedSize</strong> invariant, define a helper function:</p>
<pre><code>mkBranch :: Tree a -&gt; a -&gt; Tree a -&gt; Tree a</code></pre>
<p>Tip 2: the <strong>Balanced</strong> invariant is the hardest to maintain, so implement it last. Search for “tree rotation”.</p></li>
</ol>
<h2 id="task-4-1">Task 4</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T4</code>.</p></li>
<li><p>Using the <code>Tree</code> data type from <code>HW1.T3</code>, define the following function:</p>
<pre><code>tfoldr :: (a -&gt; b -&gt; b) -&gt; b -&gt; Tree a -&gt; b</code></pre>
<p>It must collect the elements in order:</p>
<pre><code>treeToList :: Tree a -&gt; [a]    -- output list is sorted
treeToList = tfoldr (:) []</code></pre>
<p>This follows from the <strong>Sorted</strong> invariant.</p>
<p>You are encouraged to define <code>tfoldr</code> in an efficient manner, doing only a single pass over the tree and without constructing intermediate lists.</p></li>
</ol>
<h2 id="task-5-1">Task 5</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T5</code>.</p></li>
<li><p>Implement the following function:</p>
<pre><code>splitOn :: Eq a =&gt; a -&gt; [a] -&gt; NonEmpty [a]</code></pre>
<p>Conceptually, it splits a list into sublists by a separator:</p>
<pre><code>ghci&gt; splitOn &#39;/&#39; &quot;path/to/file&quot;
[&quot;path&quot;, &quot;to&quot;, &quot;file&quot;]

ghci&gt; splitOn &#39;/&#39; &quot;path/with/trailing/slash/&quot;
[&quot;path&quot;, &quot;with&quot;, &quot;trailing&quot;, &quot;slash&quot;, &quot;&quot;]</code></pre>
<p>Due to the use of <code>NonEmpty</code> to enforce that there is at least one sublist in the output, the actual GHCi result will look slightly differently:</p>
<pre><code>ghci&gt; splitOn &#39;/&#39; &quot;path/to/file&quot;
&quot;path&quot; :| [&quot;to&quot;,&quot;file&quot;]</code></pre>
<p>Do not let that confuse you. The first element is not in any way special.</p></li>
<li><p>Implement the following function:</p>
<pre><code>joinWith :: a -&gt; NonEmpty [a] -&gt; [a]</code></pre>
<p>It must be the inverse of <code>splitOn</code>, so that:</p>
<pre><code>(joinWith sep . splitOn sep)  ≡  id</code></pre>
<p>Example usage:</p>
<pre><code>ghci&gt; &quot;import &quot; ++ joinWith &#39;.&#39; (&quot;Data&quot; :| &quot;List&quot; : &quot;NonEmpty&quot; : [])
&quot;import Data.List.NonEmpty&quot;</code></pre></li>
</ol>
<h2 id="task-6-1">Task 6</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T6</code>.</p></li>
<li><p>Using <code>Foldable</code> methods <em>only</em>, implement the following function:</p>
<pre><code>mcat :: Monoid a =&gt; [Maybe a] -&gt; a</code></pre>
<p>Example usage:</p>
<pre><code>ghci&gt; mcat [Just &quot;mo&quot;, Nothing, Nothing, Just &quot;no&quot;, Just &quot;id&quot;]
&quot;monoid&quot;

ghci&gt; Data.Monoid.getSum $ mcat [Nothing, Just 2, Nothing, Just 40]
42</code></pre></li>
<li><p>Using <code>foldMap</code> to consume the list, implement the following function:</p>
<pre><code>epart :: (Monoid a, Monoid b) =&gt; [Either a b] -&gt; (a, b)</code></pre>
<p>Example usage:</p>
<pre><code>ghci&gt; epart [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
(Sum {getSum = 8},[1,2,3,4,5])</code></pre></li>
</ol>
<h2 id="task-7">Task 7</h2>
<ol type="1">
<li><p>Create a module named <code>HW1.T7</code>.</p></li>
<li><p>Define the following data type and a lawful <code>Semigroup</code> instance for it:</p>
<pre><code>data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+</code></pre></li>
<li><p>Define the following data type and a lawful <code>Semigroup</code> instance for it:</p>
<pre><code>data Inclusive a b = This a | That b | Both a b</code></pre>
<p>The instance must not discard any values:</p>
<pre><code>This i  &lt;&gt;  This j  =  This (i &lt;&gt; j)   -- OK
This i  &lt;&gt;  This _  =  This i          -- This is not the Semigroup you&#39;re looking for.</code></pre></li>
<li><p>Define the following data type:</p>
<pre><code>newtype DotString = DS String</code></pre>
<p>Implement a <code>Semigroup</code> instance for it, such that the strings are concatenated with a dot:</p>
<pre><code>ghci&gt; DS &quot;person&quot; &lt;&gt; DS &quot;address&quot; &lt;&gt; DS &quot;city&quot;
DS &quot;person.address.city&quot;</code></pre>
<p>Implement a <code>Monoid</code> instance for it using <code>DS ""</code> as the identity element. Make sure that the laws hold:</p>
<pre><code>mempty &lt;&gt; a  ≡  a
a &lt;&gt; mempty  ≡  a</code></pre></li>
<li><p>Define the following data type:</p>
<pre><code>newtype Fun a = F (a -&gt; a)</code></pre>
<p>Implement lawful <code>Semigroup</code> and <code>Monoid</code> instances for it.</p></li>
</ol>
<h1 id="homework-2">Homework #2</h1>
<h2 id="task-1-2">Task 1</h2>
<ol type="1">
<li><p>Create a module named <code>HW2.T1</code> and define the following data types in it:</p>
<ul>
<li><pre><code>  data Option a = None | Some a</code></pre></li>
<li><pre><code>  data Pair a = P a a</code></pre></li>
<li><pre><code>  data Quad a = Q a a a a</code></pre></li>
<li><pre><code>  data Annotated e a = a :# e
  infix 0 :#</code></pre></li>
<li><pre><code>  data Except e a = Error e | Success a</code></pre></li>
<li><pre><code>  data Prioritised a = Low a | Medium a | High a</code></pre></li>
<li><pre><code>  data Stream a = a :&gt; Stream a
  infixr 5 :&gt;</code></pre></li>
<li><pre><code>  data List a = Nil | a :. List a
  infixr 5 :.</code></pre></li>
<li><pre><code>  data Fun i a = F (i -&gt; a)</code></pre></li>
<li><pre><code>  data Tree a = Leaf | Branch (Tree a) a (Tree a)</code></pre></li>
</ul></li>
<li><p>For each of those types, implement a function of the following form:</p>
<pre><code>mapF :: (a -&gt; b) -&gt; (F a -&gt; F b)</code></pre>
<p>That is, implement the following functions:</p>
<pre><code>mapOption      :: (a -&gt; b) -&gt; (Option a -&gt; Option b)
mapPair        :: (a -&gt; b) -&gt; (Pair a -&gt; Pair b)
mapQuad        :: (a -&gt; b) -&gt; (Quad a -&gt; Quad b)
mapAnnotated   :: (a -&gt; b) -&gt; (Annotated e a -&gt; Annotated e b)
mapExcept      :: (a -&gt; b) -&gt; (Except e a -&gt; Except e b)
mapPrioritised :: (a -&gt; b) -&gt; (Prioritised a -&gt; Prioritised b)
mapStream      :: (a -&gt; b) -&gt; (Stream a -&gt; Stream b)
mapList        :: (a -&gt; b) -&gt; (List a -&gt; List b)
mapFun         :: (a -&gt; b) -&gt; (Fun i a -&gt; Fun i b)
mapTree        :: (a -&gt; b) -&gt; (Tree a -&gt; Tree b)</code></pre>
<p>These functions must modify only the elements and preserve the overall structure (e.g. do not reverse the list, do not rebalance the tree, do not swap the pair).</p>
<p>This property is witnessed by the following laws:</p>
<pre><code>         mapF id  ≡  id
mapF f ∘ mapF g   ≡  mapF (f ∘ g)</code></pre>
<p>You must implement these functions by hand, without using any predefined functions (not even from <code>Prelude</code>) or deriving.</p></li>
</ol>
<h2 id="task-2-2">Task 2</h2>
<p>Create a module named <code>HW2.T2</code>. For each type from the first task except <code>Tree</code>, implement functions of the following form:</p>
<pre><code>distF :: (F a, F b) -&gt; F (a, b)
wrapF :: a -&gt; F a</code></pre>
<p>That is, implement the following functions:</p>
<pre><code>distOption      :: (Option a, Option b) -&gt; Option (a, b)
distPair        :: (Pair a, Pair b) -&gt; Pair (a, b)
distQuad        :: (Quad a, Quad b) -&gt; Quad (a, b)
distAnnotated   :: Semigroup e =&gt; (Annotated e a, Annotated e b) -&gt; Annotated e (a, b)
distExcept      :: (Except e a, Except e b) -&gt; Except e (a, b)
distPrioritised :: (Prioritised a, Prioritised b) -&gt; Prioritised (a, b)
distStream      :: (Stream a, Stream b) -&gt; Stream (a, b)
distList        :: (List a, List b) -&gt; List (a, b)
distFun         :: (Fun i a, Fun i b) -&gt; Fun i (a, b)</code></pre>
<pre><code>wrapOption      :: a -&gt; Option a
wrapPair        :: a -&gt; Pair a
wrapQuad        :: a -&gt; Quad a
wrapAnnotated   :: Monoid e =&gt; a -&gt; Annotated e a
wrapExcept      :: a -&gt; Except e a
wrapPrioritised :: a -&gt; Prioritised a
wrapStream      :: a -&gt; Stream a
wrapList        :: a -&gt; List a
wrapFun         :: a -&gt; Fun i a</code></pre>
<p>The following laws must hold:</p>
<ul>
<li><p>Homomorphism:</p>
<pre><code>distF (wrapF a, wrapF b)  ≅  wrapF (a, b)</code></pre></li>
<li><p>Associativity:</p>
<pre><code>distF (p, distF (q, r))   ≅  distF (distF (p, q), r)</code></pre></li>
<li><p>Left and right identity:</p>
<pre><code>distF (wrapF (), q)  ≅  q
distF (p, wrapF ())  ≅  p</code></pre></li>
</ul>
<p>In the laws stated above, we reason up to the following isomorphisms:</p>
<pre><code>((a, b), c)  ≅  (a, (b, c))  -- for associativity
    ((), b)  ≅  b            -- for left identity
    (a, ())  ≅  a            -- for right identity</code></pre>
<p>There is more than one way to implement some of these functions. In addition to the laws, take the following expectations into account:</p>
<ul>
<li><code>distPrioritised</code> must pick the higher priority out of the two.</li>
<li><code>distList</code> must associate each element of the first list with each element of the second list (i.e. the resulting list is of length <code>n × m</code>).</li>
</ul>
<p>You must implement these functions by hand, using only:</p>
<ul>
<li>data types that you defined in <code>HW2.T1</code></li>
<li><code>(&lt;&gt;)</code> and <code>mempty</code> for <code>Annotated</code></li>
</ul>
<h2 id="task-3-2">Task 3</h2>
<p>Create a module named <code>HW2.T3</code>. For <code>Option</code>, <code>Except</code>, <code>Annotated</code>, <code>List</code>, and <code>Fun</code> define a function of the following form:</p>
<pre><code>joinF :: F (F a) -&gt; F a</code></pre>
<p>That is, implement the following functions:</p>
<pre><code>joinOption    :: Option (Option a) -&gt; Option a
joinExcept    :: Except e (Except e a) -&gt; Except e a
joinAnnotated :: Semigroup e =&gt; Annotated e (Annotated e a) -&gt; Annotated e a
joinList      :: List (List a) -&gt; List a
joinFun       :: Fun i (Fun i a) -&gt; Fun i a</code></pre>
<p>The following laws must hold:</p>
<ul>
<li><p>Associativity:</p>
<pre><code>joinF (mapF joinF m)  ≡  joinF (joinF m)</code></pre>
<p>In other words, given <code>F (F (F a))</code>, it does not matter whether we join the outer layers or the inner layers first.</p></li>
<li><p>Left and right identity:</p>
<pre><code>joinF      (wrapF m)  ≡  m
joinF (mapF wrapF m)  ≡  m</code></pre>
<p>In other words, layers created by <code>wrapF</code> are identity elements to <code>joinF</code>.</p>
<p>Given <code>F a</code>, you can add layers outside/inside to get <code>F (F a)</code>, but <code>joinF</code> flattens it back into <code>F a</code> without any other changes to the structure.</p></li>
</ul>
<p>Furthermore, <code>joinF</code> is strictly more powerful than <code>distF</code> and can be used to define it:</p>
<pre><code>distF (p, q) = joinF (mapF (\a -&gt; mapF (\b -&gt; (a, b)) q) p)</code></pre>
<p>At the same time, this is only one of the possible <code>distF</code> definitions (e.g. <code>List</code> admits at least two lawful <code>distF</code>). It is common in Haskell to expect <code>distF</code> and <code>joinF</code> to agree in behavior, so the above equation must hold. (Do not redefine <code>distF</code> using <code>joinF</code>, though: it would be correct but not the point of the exercise).</p>
<h2 id="task-4-2">Task 4</h2>
<ol type="1">
<li><p>Create a module named <code>HW2.T4</code> and define the following data type in it:</p>
<pre><code>data State s a = S { runS :: s -&gt; Annotated s a }</code></pre></li>
<li><p>Implement the following functions:</p>
<pre><code>mapState :: (a -&gt; b) -&gt; State s a -&gt; State s b
wrapState :: a -&gt; State s a
joinState :: State s (State s a) -&gt; State s a
modifyState :: (s -&gt; s) -&gt; State s ()</code></pre>
<p>Using those functions, define <code>Functor</code>, <code>Applicative</code>, and <code>Monad</code> instances:</p>
<pre><code>instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p &lt;*&gt; q = Control.Monad.ap p q

instance Monad (State s) where
  m &gt;&gt;= f = joinState (fmap f m)</code></pre>
<p>These instances will enable the use of <code>do</code>-notation with <code>State</code>.</p>
<p>The semantics of <code>State</code> are such that the following holds:</p>
<pre><code>runS (do modifyState f; modifyState g; return a) x
  ≡
a :# g (f x)</code></pre>
<p>In other words, we execute stateful actions left-to-right, passing the state from one to another.</p></li>
<li><p>Define the following data type, representing a small language:</p>
<pre><code>data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)</code></pre>
<p>For notational convenience, define the following instances:</p>
<pre><code>instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  ...
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  ...</code></pre>
<p>So that <code>(3.14 + 1.618 :: Expr)</code> produces this syntax tree:</p>
<pre><code>Op (Add (Val 3.14) (Val 1.618))</code></pre></li>
<li><p>Using <code>do</code>-notation for <code>State</code> and combinators we defined for it (<code>pure</code>, <code>modifyState</code>), define the evaluation function:</p>
<pre><code>eval :: Expr -&gt; State [Prim Double] Double</code></pre>
<p>In addition to the final result of evaluating an expression, it accumulates a trace of all individual operations:</p>
<pre><code>runS (eval (2 + 3 * 5 - 7)) []
  ≡
10 :# [Sub 17 7, Add 2 15, Mul 3 5]</code></pre>
<p>The head of the list is the last operation, this way adding another operation to the trace is O(1).</p>
<p>You can use the trace to observe the evaluation order. Consider this expression:</p>
<pre><code>(a * b) + (x * y)</code></pre>
<p>In <code>eval</code>, we choose to evaluate <code>(a * b)</code> first and <code>(x * y)</code> second, even though the opposite is also possible and would not affect the final result of the computation.</p></li>
</ol>
<h2 id="task-5-2">Task 5</h2>
<ol type="1">
<li><p>Create a module named <code>HW2.T5</code> and define the following data type in it:</p>
<pre><code>data ExceptState e s a = ES { runES :: s -&gt; Except e (Annotated s a) }</code></pre>
<p>This type is a combination of <code>Except</code> and <code>State</code>, allowing a stateful computation to abort with an error.</p></li>
<li><p>Implement the following functions:</p>
<pre><code>mapExceptState :: (a -&gt; b) -&gt; ExceptState e s a -&gt; ExceptState e s b
wrapExceptState :: a -&gt; ExceptState e s a
joinExceptState :: ExceptState e s (ExceptState e s a) -&gt; ExceptState e s a
modifyExceptState :: (s -&gt; s) -&gt; ExceptState e s ()
throwExceptState :: e -&gt; ExceptState e s a</code></pre>
<p>Using those functions, define <code>Functor</code>, <code>Applicative</code>, and <code>Monad</code> instances.</p></li>
<li><p>Using <code>do</code>-notation for <code>ExceptState</code> and combinators we defined for it (<code>pure</code>, <code>modifyExceptState</code>, <code>throwExceptState</code>), define the evaluation function:</p>
<pre><code>data EvaluationError = DivideByZero
eval :: Expr -&gt; ExceptState EvaluationError [Prim Double] Double</code></pre>
<p>It works just as <code>eval</code> from the previous task but aborts the computation if division by zero occurs:</p>
<ul>
<li><pre><code>  runES (eval (2 + 3 * 5 - 7)) []
    ≡
  Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5])</code></pre></li>
<li><pre><code>  runES (eval (1 / (10 - 5 * 2))) []
    ≡
  Error DivideByZero</code></pre></li>
</ul></li>
</ol>
<h2 id="task-6-2">Task 6</h2>
<ol type="1">
<li><p>Create a module named <code>HW2.T6</code> and define the following data type in it:</p>
<pre><code>data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)</code></pre>
<p>Here we use <code>ExceptState</code> for an entirely different purpose: to parse data from a string. Our state consists of a <code>Natural</code> representing how many characters we have already consumed (for error messages) and the <code>String</code> is the remainder of the input.</p></li>
<li><p>Implement the following function:</p>
<pre><code>runP :: Parser a -&gt; String -&gt; Except ParseError a</code></pre></li>
<li><p>Let us define a parser that consumes a single character:</p>
<pre><code>pChar :: Parser Char
pChar = P $ ES \(pos, s) -&gt;
  case s of
    []     -&gt; Error (ErrorAtPos pos)
    (c:cs) -&gt; Success (c :# (pos + 1, cs))</code></pre>
<p>Study this definition:</p>
<ul>
<li>What happens when the string is empty?</li>
<li>How does the parser state change when a character is consumed?</li>
</ul>
<p>Write a comment that explains <code>pChar</code>.</p></li>
<li><p>Implement a parser that always fails:</p>
<pre><code>parseError :: Parser a</code></pre>
<p>Define the following instance:</p>
<pre><code>instance Alternative Parser where
  empty = parseError
  (&lt;|&gt;) = ...

instance MonadPlus Parser   -- No methods.</code></pre>
<p>So that <code>p &lt;|&gt; q</code> tries to parse the input string using <code>p</code>, but in case of failure tries <code>q</code>.</p>
<p>Make sure that the laws hold:</p>
<pre><code>empty &lt;|&gt; p  ≡  p
p &lt;|&gt; empty  ≡  p</code></pre></li>
<li><p>Implement a parser that checks that there is no unconsumed input left (i.e. the string in the parser state is empty), and fails otherwise:</p>
<pre><code>pEof :: Parser ()</code></pre></li>
<li><p>Study the combinators provided by <code>Control.Applicative</code> and <code>Control.Monad</code>. The following are of particular interest:</p>
<ul>
<li><code>msum</code></li>
<li><code>mfilter</code></li>
<li><code>optional</code></li>
<li><code>many</code></li>
<li><code>some</code></li>
<li><code>void</code></li>
</ul>
<p>We can use them to construct more interesting parsers. For instance, here is a parser that accepts only non-empty sequences of uppercase letters:</p>
<pre><code>pAbbr :: Parser String
pAbbr = do
  abbr &lt;- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr</code></pre>
<p>It can be used as follows:</p>
<pre><code>ghci&gt; runP pAbbr &quot;HTML&quot;
Success &quot;HTML&quot;

ghci&gt; runP pAbbr &quot;JavaScript&quot;
Error (ErrorAtPos 1)</code></pre></li>
<li><p>Using parser combinators, define the following function:</p>
<pre><code>parseExpr :: String -&gt; Except ParseError Expr</code></pre>
<p>It must handle floating-point literals of the form <code>4.09</code>, the operators <code>+</code> <code>-</code> <code>*</code> <code>/</code> with the usual precedence (multiplication and division bind tighter than addition and subtraction), and parentheses.</p>
<p>Example usage:</p>
<ul>
<li><pre><code>  ghci&gt; parseExpr &quot;3.14 + 1.618 * 2&quot;
  Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))</code></pre></li>
<li><pre><code>  ghci&gt; parseExpr &quot;2 * (1 + 3)&quot;
  Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0)))))</code></pre></li>
<li><pre><code>  ghci&gt; parseExpr &quot;24 + Hello&quot;
  Error (ErrorAtPos 3)</code></pre></li>
</ul>
<p>The implementation must not use the <code>Read</code> class, as it implements similar functionality (the exercise is to write your own parsers). At the same time, you are encouraged to use existing <code>Applicative</code> and <code>Monad</code> combinators, since they are not specific to parsing.</p></li>
</ol></main>
  </body>
</html>
