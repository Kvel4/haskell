
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
<p>In <code>daysToParty</code>, if it is already <code>Friday</code>, the party can start immediately, we don???t have to wait for the next week (i.e. return <code>0</code> rather than <code>7</code>).</p>
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
<p>Tip 2: the <strong>Balanced</strong> invariant is the hardest to maintain, so implement it last. Search for ???tree rotation???.</p></li>
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
<pre><code>(joinWith sep . splitOn sep)  ???  id</code></pre>
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
<pre><code>mempty &lt;&gt; a  ???  a
a &lt;&gt; mempty  ???  a</code></pre></li>
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
<pre><code>         mapF id  ???  id
mapF f ??? mapF g   ???  mapF (f ??? g)</code></pre>
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
<pre><code>distF (wrapF a, wrapF b)  ???  wrapF (a, b)</code></pre></li>
<li><p>Associativity:</p>
<pre><code>distF (p, distF (q, r))   ???  distF (distF (p, q), r)</code></pre></li>
<li><p>Left and right identity:</p>
<pre><code>distF (wrapF (), q)  ???  q
distF (p, wrapF ())  ???  p</code></pre></li>
</ul>
<p>In the laws stated above, we reason up to the following isomorphisms:</p>
<pre><code>((a, b), c)  ???  (a, (b, c))  -- for associativity
    ((), b)  ???  b            -- for left identity
    (a, ())  ???  a            -- for right identity</code></pre>
<p>There is more than one way to implement some of these functions. In addition to the laws, take the following expectations into account:</p>
<ul>
<li><code>distPrioritised</code> must pick the higher priority out of the two.</li>
<li><code>distList</code> must associate each element of the first list with each element of the second list (i.e. the resulting list is of length <code>n ?? m</code>).</li>
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
<pre><code>joinF (mapF joinF m)  ???  joinF (joinF m)</code></pre>
<p>In other words, given <code>F (F (F a))</code>, it does not matter whether we join the outer layers or the inner layers first.</p></li>
<li><p>Left and right identity:</p>
<pre><code>joinF      (wrapF m)  ???  m
joinF (mapF wrapF m)  ???  m</code></pre>
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
  ???
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
  ???
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
    ???
  Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5])</code></pre></li>
<li><pre><code>  runES (eval (1 / (10 - 5 * 2))) []
    ???
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
<pre><code>empty &lt;|&gt; p  ???  p
p &lt;|&gt; empty  ???  p</code></pre></li>
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
<h1 id="homework-3">Homework #3</h1>
<p>In this homework we will gradually develop a small programming language called Hi.</p>
<h2 id="project-structure">Project Structure</h2>
<p>Create a <code>.cabal</code> file with both a library and an executable:</p>
<pre><code>library
  exposed-modules:    ...
  build-depends:      ...
  ...

executable hi
  main-is:            Main.hs
  hs-source-dirs:     ...
  build-depends:      ...
  ...</code></pre>
<p>In the library component, create the following modules:</p>
<pre><code>HW3.Base
HW3.Parser
HW3.Pretty
HW3.Evaluator</code></pre>
<p>You are allowed to add more modules to the project, but those are the required ones.</p>
<ul>
<li><p>In <code>HW3.Base</code>, define the following data types:</p>
<pre><code>data HiFun     -- function names (e.g. div, sort, length, ...)
data HiValue   -- values (numbers, booleans, strings, ...)
data HiExpr    -- expressions (literals, function calls, ...)
data HiError   -- evaluation errors (invalid arguments, ...)</code></pre>
<p>In each task, we will add constructors to these data types that are needed to implement new language features.</p></li>
<li><p>In <code>HW3.Parser</code>, define the following function:</p>
<pre><code>parse :: String -&gt; Either (ParseErrorBundle String Void) HiExpr</code></pre>
<p>The <code>ParseErrorBundle</code> type comes from the <code>megaparsec</code> package which we will use to implement our parser.</p></li>
<li><p>In <code>HW3.Pretty</code>, define the following function:</p>
<pre><code>prettyValue :: HiValue -&gt; Doc AnsiStyle</code></pre>
<p>The <code>Doc</code> and <code>AnsiStyle</code> types come from the <code>prettyprinter</code> and <code>prettyprinter-ansi-terminal</code> packages respectively. This function renders a value to a document, which in turn can be either printed to the terminal (with color highlighting) or converted to a string.</p></li>
<li><p>In <code>HW3.Evaluator</code>, define the following function:</p>
<pre><code>eval :: Monad m =&gt; HiExpr -&gt; m (Either HiError HiValue)</code></pre>
<p>One might wonder why we need the <code>Monad m</code> part. Indeed, for arithmetic operations, a simpler type would be sufficient:</p>
<pre><code>eval :: HiExpr -&gt; Either HiError HiValue</code></pre>
<p>However, the monadic context will come into play later, when we start implementing IO actions (file system access, random number generation, and so on).</p></li>
</ul>
<p>The executable component consists just of a single file, <code>Main.hs</code>.</p>
<h2 id="the-repl">The REPL</h2>
<p>Using the <code>haskeline</code> package, implement a REPL in <code>Main.hs</code> that uses <code>parse</code>, <code>eval</code>, and <code>prettyValue</code> defined above. It???s going to be just 15???20 lines of code, but you will use it all the time to test your implementation.</p>
<p>Here???s an example session that will become possible as soon as we implement arithmetic operations:</p>
<pre><code>hi&gt; mul(2, 10)
20

hi&gt; sub(1000, 7)
993

hi&gt; div(3, 5)
0.6</code></pre>
<h2 id="task-1-numbers-and-arithmetic">Task 1: Numbers and arithmetic</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub

data HiValue =
  ...
  | HiValueNumber Rational
  | HiValueFunction HiFun

data HiExpr =
  ...
  | HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]

data HiError =
  ...
  | HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero</code></pre>
<p>Numbers are represented using the <code>Rational</code> type from <code>Data.Ratio</code>.</p></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in names <code>div</code>, <code>mul</code>, <code>add</code>, and <code>sub</code>, that are parsed into the corresponding <code>HiFun</code> constructors (and then wrapped in <code>HiValueFunction</code>).</p></li>
<li><p>Numeric literals, such as <code>2</code>, <code>3.14</code>, <code>-1.618</code>, or <code>1.2e5</code>, that are parsed into <code>HiValueNumber</code> (tip: use <code>Text.Megaparsec.Char.Lexer.scientific</code>).</p></li>
<li><p>Function application <code>f(a, b, c, ...)</code> that is parsed into <code>HiExprApply</code>.</p></li>
</ul>
<p>For example, the expression <code>div(add(10, 15.1), 3)</code> is represented by the following syntax tree:</p>
<pre><code>HiExprApply (HiExprValue (HiValueFunction HiFunDiv))
  [
    HiExprApply (HiExprValue (HiValueFunction HiFunAdd))
      [
        HiExprValue (HiValueNumber (10 % 1)),
        HiExprValue (HiValueNumber (151 % 10))
      ],
    HiExprValue (HiValueNumber (3 % 1))
  ]</code></pre></li>
<li><p>In the evaluator, implement the arithmetic operations:</p>
<ul>
<li><code>add(500, 12)</code> evaluates to <code>512</code> (addition)</li>
<li><code>sub(10, 100)</code> evaluates to <code>-90</code> (subtraction)</li>
<li><code>mul(23, 768)</code> evaluates to <code>17664</code> (multiplication)</li>
<li><code>div(57, 190)</code> evaluates to <code>0.3</code> (division)</li>
</ul>
<p>Nested function applications are allowed:</p>
<ul>
<li><code>div(add(mul(2, 5), 1), sub(11,6))</code> evaluates to <code>2.2</code></li>
</ul>
<p>The following errors must be returned as <code>HiError</code>:</p>
<ul>
<li><code>HiErrorArityMismatch</code>: functions called with an incorrect amount of arguments, e.g. <code>sub(1)</code> or <code>sub(1, 2, 3)</code>.</li>
<li><code>HiErrorDivideByZero</code>: the <code>div</code> function is called with <code>0</code> as its second argument, e.g. <code>div(1, 0)</code> or <code>div(1, sub(5, 5))</code>.</li>
<li><code>HiErrorInvalidFunction</code>: numbers are used in function positions, e.g. <code>15(2)</code>.</li>
<li><code>HiErrorInvalidArgument</code>: functions are used in numeric positions, e.g. <code>sub(10, add)</code>.</li>
</ul>
<p>You are advised to use the <code>ExceptT</code> monad transformer to propagate <code>HiError</code> through the evaluator.</p></li>
<li><p>In the pretty-printer, define the following special cases for rendering numbers:</p>
<ul>
<li>integers: <code>42</code>, <code>-8</code>, <code>15</code></li>
<li>finite decimal fractions: <code>3.14</code>, <code>-8.15</code>, <code>77.01</code></li>
<li>fractions: <code>1/3</code>, <code>-1/7</code>, <code>3/11</code></li>
<li>mixed fractions: <code>5 + 1/3</code>, <code>-10 - 1/7</code>, <code>24 + 3/11</code></li>
</ul>
<p>You will find these functions useful:</p>
<ul>
<li><code>quotRem</code> from <code>Prelude</code></li>
<li><code>fromRationalRepetendUnlimited</code> from the <code>scientific</code> package</li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible if you have implemented all of the above correctly:</p>
<pre><code>hi&gt; 100
100

hi&gt; -15
-15

hi&gt; add(100, -15)
85

hi&gt; add(3, div(14, 100))
3.14

hi&gt; div(10, 3)
3 + 1/3

hi&gt; sub(mul(201, 11), 0.33)
2210.67</code></pre>
<h2 id="task-2-booleans-and-comparison">Task 2: Booleans and comparison</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf

data HiValue =
  ...
  | HiValueBool Bool</code></pre></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in names <code>not</code>, <code>and</code>, <code>or</code>, <code>less-than</code>, <code>greater-than</code>, <code>equals</code>, <code>not-less-than</code>, <code>not-greater-than</code>, <code>not-equals</code>, <code>if</code>, that are parsed into the corresponding <code>HiFun</code> constructors.</p></li>
<li><p>Built-in names <code>true</code> and <code>false</code> that are parsed into <code>HiValueBool</code>.</p></li>
</ul></li>
<li><p>In the evaluator, implement the new operations.</p>
<p>Boolean algebra:</p>
<ul>
<li><code>not(true)</code> evaluates to <code>false</code> (negation)</li>
<li><code>and(true, false)</code> evaluates to <code>false</code> (conjunction)</li>
<li><code>or(true, false)</code> evaluates to <code>true</code> (disjunction)</li>
</ul>
<p>Equality checking:</p>
<ul>
<li><code>equals(10, 10)</code> evaluates to <code>true</code></li>
<li><code>equals(false, false)</code> evaluates to <code>true</code></li>
<li><code>equals(3, 10)</code> evaluates to <code>false</code></li>
<li><code>equals(1, true)</code> evaluates to <code>false</code> (no implicit cast)</li>
</ul>
<p>Comparisons:</p>
<ul>
<li><code>less-than(3, 10)</code> evaluates to <code>true</code></li>
<li><code>less-than(false, true)</code> evaluates to <code>true</code></li>
<li><code>less-than(false, 0)</code> evaluates to <code>true</code> (<code>Bool</code> is less than <code>Number</code>)</li>
</ul>
<p>Complements:</p>
<ul>
<li>for all <code>A</code> <code>B</code>, <code>greater-than(A, B) ??? less-than(B, A)</code> holds</li>
<li>for all <code>A</code> <code>B</code>, <code>not-equals(A, B) ??? not(equals(A, B))</code> holds</li>
<li>for all <code>A</code>, <code>B</code>, <code>not-less-than(A, B) ??? not(less-than(A, B))</code> holds</li>
<li>for all <code>A</code>, <code>B</code>, <code>not-greater-than(A, B) ??? not(greater-than(A, B))</code> holds</li>
</ul>
<p>Branching:</p>
<ul>
<li>for all <code>A</code> <code>B</code>, <code>if(true, A, B) ??? A</code> holds</li>
<li>for all <code>A</code> <code>B</code>, <code>if(false, A, B) ??? B</code> holds</li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; false
false

hi&gt; equals(add(2, 2), 4)
true

hi&gt; less-than(mul(999, 99), 10000)
false

hi&gt; if(greater-than(div(2, 5), div(3, 7)), 1, -1)
-1

hi&gt; and(less-than(0, 1), less-than(1, 0))
false</code></pre>
<p>Note also that functions are values:</p>
<pre><code>hi&gt; if(true, add, mul)
add

hi&gt; if(true, add, mul)(10, 10)
20

hi&gt; if(false, add, mul)(10, 10)
100</code></pre>
<p>Functions can also be tested for equality:</p>
<pre><code>hi&gt; equals(add, add)
true

hi&gt; equals(add, mul)
false</code></pre>
<p>The check is trivial: a function is equal only to itself.</p>
<p><small>Ordering of function symbols is implementation-defined, that is, it???s up to you whether <code>less-than(add, mul)</code> or <code>greater-than(add, mul)</code>.</small></p>
<h2 id="task-3-operators">Task 3: Operators</h2>
<p>In the parser, add support for infix operators. The precedence and associativity are the same as in Haskell.</p>
<p>For all <code>A B</code>:</p>
<ul>
<li><code>A / B</code> parses to <code>div(A, B)</code></li>
<li><code>A * B</code> parses to <code>mul(A, B)</code></li>
<li><code>A + B</code> parses to <code>add(A, B)</code></li>
<li><code>A - B</code> parses to <code>sub(A, B)</code></li>
<li><code>A &lt; B</code> parses to <code>less-than(A, B)</code></li>
<li><code>A &gt; B</code> parses to <code>greater-than(A, B)</code></li>
<li><code>A &gt;= B</code> parses to <code>not-less-than(A, B)</code></li>
<li><code>A &lt;= B</code> parses to <code>not-greater-than(A, B)</code></li>
<li><code>A == B</code> parses to <code>equals(A, B)</code></li>
<li><code>A /= B</code> parses to <code>not-equals(A, B)</code></li>
<li><code>A &amp;&amp; B</code> parses to <code>and(A, B)</code></li>
<li><code>A || B</code> parses to <code>or(A, B)</code></li>
</ul>
<p>Tip: use <code>makeExprParser</code> from the <code>parser-combinators</code> package.</p>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; 2 + 2
4

hi&gt; 2 + 2 * 3
8

hi&gt; (2 + 2) * 3
12

hi&gt; 2 + 2 * 3 == (2 + 2) * 3
false

hi&gt; 10 == 2*5 &amp;&amp; 143 == 11*13
true</code></pre>
<h2 id="task-4-strings-and-slices">Task 4: Strings and slices</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim

data HiValue =
  ...
  | HiValueNull
  | HiValueString Text</code></pre>
<p>Strings are represented using the <code>Text</code> type from the <code>text</code> package.</p></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Bulit-in names <code>length</code>, <code>to-upper</code>, <code>to-lower</code>, <code>reverse</code>, <code>trim</code>, that are parsed into the corresponding <code>HiFun</code> constructors.</p></li>
<li><p>Built-in name <code>null</code> that is parsed into <code>HiValueNull</code>.</p></li>
<li><p>String literals, such as <code>"hello"</code>, <code>"42"</code>, or <code>"header\nfooter"</code>, that are parsed into <code>HiValueString</code> (tip: use <code>Text.Megaparsec.Char.Lexer.charLiteral</code>).</p></li>
</ul></li>
<li><p>In the evaluator, implement the new operations:</p>
<ul>
<li><code>length("Hello World")</code> evaluates to <code>11</code></li>
<li><code>to-upper("Hello World")</code> evaluates to <code>"HELLO WORLD"</code></li>
<li><code>to-lower("Hello World")</code> evaluates to <code>"hello world"</code></li>
<li><code>reverse("stressed")</code> evaluates to <code>"desserts"</code></li>
<li><code>trim(" Hello World ")</code> evaluates to <code>"Hello World"</code></li>
</ul>
<p>Then overload existing operations to work on strings:</p>
<ul>
<li><code>"Hello" + "World"</code> evaluates to <code>"HelloWorld"</code></li>
<li><code>"Cat" * 5</code> evaluates to <code>"CatCatCatCatCat"</code> (tip: use <code>stimes</code>)</li>
<li><code>"/home/user" / "hi"</code> evaluates to <code>"/home/user/hi"</code></li>
</ul>
<p>When a string is used as a function of one argument, perform a lookup:</p>
<ul>
<li><code>"Hello World"(0)</code> evaluates to <code>"H"</code></li>
<li><code>"Hello World"(7)</code> evaluates to <code>"o"</code></li>
</ul>
<p>Out-of-bounds indexing returns <code>null</code>:</p>
<ul>
<li><code>"Hello World"(-1)</code> evaluates to <code>null</code></li>
<li><code>"Hello World"(99)</code> evaluates to <code>null</code></li>
</ul>
<p>When a string is used as a function of two arguments, take a slice:</p>
<ul>
<li><code>"Hello World"(0, 5)</code> evaluates to <code>"Hello"</code></li>
<li><code>"Hello World"(2, 4)</code> evaluates to <code>"ll"</code></li>
</ul></li>
<li><p>(Advanced) When a slice index is negative, implement the Python semantics of indexing from the end of the string:</p>
<ul>
<li><code>"Hello World"(0, -4)</code> evaluates to <code>"Hello W"</code></li>
<li><code>"Hello World"(-4, -1)</code> evaluates to <code>"orl"</code></li>
</ul>
<p>When a slice index is <code>null</code>, treat it as the start/end of the string:</p>
<ul>
<li><code>"Hello, World"(2, null)</code> evaluates to <code>"llo, World"</code></li>
<li><code>"Hello, World"(null, 5)</code> evaluates to <code>"Hello"</code></li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; to-upper(&quot;what a nice language&quot;)(7, 11)
&quot;NICE&quot;

hi&gt; &quot;Hello&quot; == &quot;World&quot;
false

hi&gt; length(&quot;Hello&quot; + &quot;World&quot;)
10

hi&gt; length(&quot;hehe&quot; * 5) / 3
6 + 2/3</code></pre>
<h2 id="task-5-lists-and-folds">Task 5: Lists and folds</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunList
  | HiFunRange
  | HiFunFold

data HiValue =
  ...
  | HiValueList (Seq HiValue)</code></pre>
<p>Lists are represented using the <code>Seq</code> type from the <code>containers</code> package.</p></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in names <code>list</code>, <code>range</code>, <code>fold</code>, that are parsed into the corresponding <code>HiFun</code> constructors.</p></li>
<li><p>List literals, written as <code>[A, B, C, ...]</code>, that are parsed into function application <code>list(A, B, C, ...)</code>.</p></li>
</ul></li>
<li><p>In the evaluator, implement the new operations:</p>
<ul>
<li><code>list(1, 2, 3)</code> constructs <code>HiValueList</code> containing <code>1</code>, <code>2</code>, <code>3</code></li>
<li><code>range(5, 10.3)</code> evaluates to <code>[5, 6, 7, 8, 9, 10]</code></li>
<li><code>fold(add, [11, 22, 33])</code> evaluates to <code>66</code></li>
<li><code>fold(mul, [11, 22, 33])</code> evaluates to <code>7986</code></li>
<li><code>fold(div, [11, 22, 33])</code> evaluates to <code>1/66</code> (left fold)</li>
</ul>
<p>Then overload existing operations to work on lists:</p>
<ul>
<li><code>length([1, true, "Hello"])</code> evaluates to <code>3</code></li>
<li><code>reverse([1, true, "Hello"])</code> evaluates to <code>["Hello", true, 1]</code></li>
<li><code>[1, 2] + [3, 4, 5]</code> evaluates to <code>[1, 2, 3, 4, 5]</code></li>
<li><code>[0, "x"] * 3</code> evaluates to <code>[0, "x", 0, "x", 0, "x"]</code> (tip: use <code>stimes</code>)</li>
</ul>
<p>When a list is used as a function, perform indexing/slicing:</p>
<ul>
<li><code>["hello", true, "world"](1)</code> evaluates to <code>true</code></li>
<li><code>["hello", true, "world"](1,3)</code> evaluates to <code>[true, "world"]</code></li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; list(1, 2, 3, 4, 5)
[ 1, 2, 3, 4, 5 ]

hi&gt; fold(add, [2, 5] * 3)
21

hi&gt; fold(mul, range(1, 10))
3628800

hi&gt; [0, true, false, &quot;hello&quot;, &quot;world&quot;](2, 4)
[ false, &quot;hello&quot; ]

hi&gt; reverse(range(0.5, 70/8))
[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]</code></pre>
<h2 id="task-6-bytes-and-serialisation">Task 6: Bytes and serialisation</h2>
<p>Lists of bytes (numbers from <code>0</code> to <code>255</code>) can be represented and processed more efficiently. Let us introduce a new value type for them.</p>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise

data HiValue =
  ...
  | HiValueBytes ByteString</code></pre>
<p>Bytes are represented using the strict <code>ByteString</code> type from the <code>bytestring</code> package.</p></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in names <code>pack-bytes</code>, <code>unpack-bytes</code>, <code>zip</code>, <code>unzip</code>, <code>encode-utf8</code>, <code>decode-utf8</code>, <code>serialise</code>, and <code>deserialise</code>, that are parsed into the corresponding <code>HiFun</code> constructors.</p></li>
<li><p>Byte array literals, such as <code>[# 01 3f ec #]</code> that are parsed into <code>HiValueBytes</code>. Each element is a two-digit hexadecimal number.</p></li>
</ul></li>
<li><p>In the evaluator, implement the new operations:</p>
<ul>
<li><code>pack-bytes([ 3, 255, 158, 32 ])</code> evaluates to <code>[# 03 ff 9e 20 #]</code></li>
<li><code>unpack-bytes([# 10 20 30 #])</code> evaluates to <code>[16, 32, 48]</code></li>
<li><code>encode-utf8("Hello!")</code> evaluates to <code>[# 48 65 6c 6c 6f 21 #]</code></li>
<li><code>decode-utf8([# 48 65 6c 6c 6f #])</code> evaluates to <code>"Hello"</code></li>
<li><code>decode-utf8([# c3 28 #])</code> evaluates to <code>null</code> (invalid UTF-8 byte sequence)</li>
<li><code>zip</code> compresses the bytes using the <code>zlib</code> package (specify <code>bestCompression</code>)</li>
<li><code>serialise</code> turns any value into bytes using the <code>serialise</code> package</li>
<li>for all <code>A</code>, <code>unzip(zip(A)) ??? A</code> holds</li>
<li>for all <code>A</code>, <code>deserialise(serialise(A)) ??? A</code> holds</li>
</ul>
<p>Then overload existing operations to work on bytes:</p>
<ul>
<li><code>[# 00 ff #] + [# 01 e3 #]</code> evaluates to <code>[# 00 ff 01 e3 #]</code></li>
<li><code>[# 00 ff #] * 3</code> evaluates to <code>[# 00 ff 00 ff 00 ff #]</code> (tip: use <code>stimes</code>)</li>
</ul>
<p>When bytes are used as a function, perform indexing/slicing as with strings and lists:</p>
<ul>
<li><code>[# 00 ff 01 e3 #](1)</code> evaluates to <code>255</code></li>
<li><code>[# 00 ff 01 e3 #](1,3)</code> evaluates to <code>[# ff 01 #]</code></li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; pack-bytes(range(30, 40))
[# 1e 1f 20 21 22 23 24 25 26 27 28 #]

hi&gt; zip(encode-utf8(&quot;Hello, World!&quot; * 1000))
[# 78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af
   fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88
   88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]

hi&gt; decode-utf8([# 68 69 #] * 5)
&quot;hihihihihi&quot;

hi&gt; unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])
[# 01 02 03 #]</code></pre>
<h2 id="task-7-file-io">Task 7: File I/O</h2>
<p>In this task we extend the language with I/O capabilities. We consider it to be the most important part of the homework and it is graded with extra points.</p>
<p>Let us start by creating a new type in <code>HW3.Base</code> that encodes the available actions:</p>
<pre><code>data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd</code></pre>
<p>Now recall that the type of our <code>eval</code> function is as follows:</p>
<pre><code>eval :: Monad m =&gt; HiExpr -&gt; m (Either HiError HiValue)</code></pre>
<p>We could just require <code>m</code> to be <code>IO</code> in order to execute the actions, but that would be bad design, as it would make it impossible to do evaluation in a pure, deterministic context (e.g. for tests). Instead, let us create a new class in <code>HW3.Base</code>:</p>
<pre><code>class Monad m =&gt; HiMonad m where
  runAction :: HiAction -&gt; m HiValue</code></pre>
<p>One could imagine at least a few possible instances of this class:</p>
<ol type="1">
<li><p><code>IO</code>, where those actions could interact with the actual file system</p></li>
<li><p><code>Identity</code>, where the actions do nothing and return <code>null</code></p></li>
<li><p><code>State FS</code>, where <code>FS</code> is a pure and deterministic in-memory simulation of the file system</p></li>
<li><p><code>ReaderT Permissions IO</code>, which can be more secure than <code>IO</code> by controlling whether the program has read-only or read-write access</p></li>
</ol>
<p>While it would be useful to implement all of those, we shall limit ourselves to the last one to avoid making the task unnecessarily tedious.</p>
<p>In a new module <code>HW3.Action</code>, declare the following:</p>
<pre><code>data HiPermission =
    AllowRead
  | AllowWrite

data PermissionException =
  PermissionRequired HiPermission

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -&gt; IO a }</code></pre>
<p>Finally, let us change the type of <code>eval</code> as follows:</p>
<div class="sourceCode" id="cb124"><pre class="sourceCode diff"><code class="sourceCode diff"><span id="cb124-1"><a href="#cb124-1" aria-hidden="true" tabindex="-1"></a><span class="st">- eval ::   Monad m =&gt; HiExpr -&gt; m (Either HiError HiValue)</span></span>
<span id="cb124-2"><a href="#cb124-2" aria-hidden="true" tabindex="-1"></a><span class="va">+ eval :: HiMonad m =&gt; HiExpr -&gt; m (Either HiError HiValue)</span></span></code></pre></div>
<p>With those preliminaries out of the way, we can start integrating the actions into the rest of the language.</p>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir

data HiValue =
  ...
  | HiValueAction HiAction

data HiExpr =
  ...
  | HiExprRun HiExpr</code></pre></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in names <code>read</code>, <code>write</code>, <code>mkdir</code>, <code>cd</code>, that are parsed into the corresponding <code>HiFun</code> constructors.</p></li>
<li><p>Built-in name <code>cwd</code> that is parsed into <code>HiValueAction HiActionCwd</code>.</p></li>
<li><p><code>E!</code> notation that is parsed into <code>HiExprRun</code>, e.g. <code>read("hello.txt")!</code>, <code>mkdir("projects")!</code>, or <code>cwd!</code>.</p></li>
</ul></li>
<li><p>In the evaluator, implement the new functions to return the corresponding actions:</p>
<ul>
<li><code>read("hi.txt")</code> evaluates to <code>read("hi.txt")</code>. While visually the same, internally the first one is <code>HiExprApply</code> and the second one is <code>HiValueAction</code>.</li>
<li><code>write("hi.txt", "Hi!")</code> evaluates to <code>write("hi.txt", [# 48 69 21 #])</code></li>
<li><code>mkdir("dir")</code> evaluates to <code>mkdir("dir")</code></li>
<li><code>cd("dir")</code> evaluates to <code>cd("dir")</code></li>
</ul>
<p>Then implement the <code>HiExprRun</code> construct, which should execute the action using <code>runAction</code> that we defined earlier.</p></li>
<li><p>Define the <code>HiMonad HIO</code> instance, such that:</p>
<ul>
<li><code>cwd!</code> returns the current working directory</li>
<li><code>cd("mydir")!</code> changes the current working directory to <code>mydir</code></li>
<li><code>read("myfile")!</code> returns the contents of <code>myfile</code> (use <code>HiValueString</code> if the contents are valid UTF-8 and <code>HiValueBytes</code> otherwise)</li>
<li><code>read("mydir")!</code> returns the directory listing of <code>mydir</code></li>
<li><code>write("myfile", "Hello")!</code> writes <code>"Hello"</code> to <code>myfile</code></li>
<li><code>mkdir("mydir")!</code> creates a new directory <code>mydir</code></li>
</ul>
<p>Use the <code>directory</code> package to implement all of the above.</p></li>
<li><p>Implement permission control in <code>HiMonad HIO</code>, so that actions throw <code>PermissionException</code> (using <code>throwIO</code>) unless they are allowed.</p>
<ul>
<li><code>AllowRead</code> enables <code>cwd</code>, <code>cd</code>, <code>read</code></li>
<li><code>AllowWrite</code> enables <code>write</code>, <code>mkdir</code></li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; mkdir(&quot;tmp&quot;)!
null

hi&gt; read(&quot;tmp&quot;)!
[]

hi&gt; mkdir(&quot;tmp/a&quot;)!
null

hi&gt; mkdir(&quot;tmp/b&quot;)!
null

hi&gt; read(&quot;tmp&quot;)!
[ &quot;a&quot;, &quot;b&quot; ]

hi&gt; write(&quot;tmp/hi.txt&quot;, &quot;Hello&quot;)!
null

hi&gt; cd(&quot;tmp&quot;)!
null

hi&gt; read(&quot;hi.txt&quot;)!
&quot;Hello&quot;</code></pre>
<p>Note that actions are just values and only <code>!</code> forces their execution:</p>
<pre><code>hi&gt; read
read

hi&gt; read(&quot;hi.txt&quot;)
read(&quot;hi.txt&quot;)

hi&gt; read(&quot;hi.txt&quot;)!
&quot;Hello&quot;</code></pre>
<h2 id="task-8-date-and-time">Task 8: Date and time</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunParseTime

data HiValue =
  ...
  | HiValueTime UTCTime

data HiAction =
  ...
  | HiActionNow</code></pre>
<p>Time is represented using the <code>UTCTime</code> type from the <code>time</code> package.</p>
<p>Extend the data types in <code>HW3.Action</code> to include the following constructors:</p>
<pre><code>data HiPermission =
  ...
  | AllowTime</code></pre></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in name <code>parse-time</code> that is parsed into the corresponding <code>HiFun</code> constructor.</p></li>
<li><p>Built-in name <code>now</code> that is parsed into the corresponding <code>HiAction</code> constructor.</p></li>
</ul></li>
<li><p>In the evaluator, implement <code>parse-time</code> using <code>readMaybe</code> to parse a <code>HiValueString</code> into a <code>HiValueTime</code>, or <code>HiValueNull</code> in case of failure.</p></li>
<li><p>In the <code>HiMonad HIO</code> instance, implement the <code>HiActionNow</code> to return the current system time. It requires the <code>AllowTime</code> permission.</p></li>
<li><p>In the evaluator, overload existing operations to work on time:</p>
<ul>
<li><code>parse-time("2021-12-15 00:00:00 UTC") + 1000</code> evaluates to <code>parse-time("2021-12-15 00:16:40 UTC")</code> (use <code>addUTCTime</code>)</li>
<li><code>parse-time("2021-12-15 00:37:51.000890793 UTC") - parse-time("2021-12-15 00:37:47.649047038 UTC")</code> evaluates to <code>3.351843755</code> (use <code>diffUTCTime</code>)</li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; now!
parse-time(&quot;2021-12-15 00:42:33.02949461 UTC&quot;)

hi&gt; parse-time(&quot;2021-01-01 00:00:00 UTC&quot;) + 365 * 24 * 60 * 60
parse-time(&quot;2022-01-01 00:00:00 UTC&quot;)</code></pre>
<h2 id="task-9-random-numbers">Task 9: Random numbers</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunRand

data HiAction =
  ...
  | HiActionRand Int Int</code></pre></li>
<li><p>In the parser, add support for the built-in name <code>rand</code> that is parsed into the corresponding <code>HiFun</code> constructor.</p></li>
<li><p>In the evaluator, implement the new function:</p>
<ul>
<li><code>rand(0, 10)</code> evaluates to <code>rand(0, 10)</code>. While visually the same, internally the first one is <code>HiExprApply</code> and the second one is <code>HiValueAction</code>.</li>
</ul></li>
<li><p>Extend the <code>HiMonad HIO</code> instance, so that:</p>
<ul>
<li><code>rand(0, 5)!</code> evaluates to <code>0</code>, <code>1</code>, <code>2</code>, <code>3</code>, <code>4</code>, or <code>5</code></li>
<li>the distribution of random numbers is uniform</li>
</ul>
<p>Tip: use the <code>random</code> package.</p></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; rand
rand

hi&gt; rand(0, 10)
rand( 0, 10 )

hi&gt; rand(0, 10)!
8

hi&gt; rand(0, 10)!
3</code></pre>
<h2 id="task-10-short-circuit-evaluation">Task 10: Short-circuit evaluation</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunEcho

data HiAction =
  ...
  | HiActionEcho Text</code></pre></li>
<li><p>In the parser, add support for the built-in name <code>echo</code> that is parsed into the corresponding <code>HiFun</code> constructor.</p></li>
<li><p>In the evaluator, implement the new function:</p>
<ul>
<li><code>echo("Hello")</code> evaluates to <code>echo("Hello")</code>. While visually the same, internally the first one is <code>HiExprApply</code> and the second one is <code>HiValueAction</code>.</li>
</ul></li>
<li><p>Extend the <code>HiMonad HIO</code> instance, so that <code>echo("Hello")!</code> prints <code>Hello</code> followed by a newline to <code>stdout</code>. It requires the <code>AllowWrite</code> permission.</p></li>
<li><p>In the evaluator, ensure that <code>if(true, A, B)</code> does not evaluate <code>B</code>, and <code>if(false, A, B)</code> does not evaluate <code>A</code>.</p>
<p>Then generalise <code>A &amp;&amp; B</code> as follows:</p>
<ul>
<li>if <code>A</code> is <code>false</code> or <code>null</code>, return <code>A</code> without evaluating <code>B</code></li>
<li>otherwise, evaluate and return <code>B</code></li>
</ul>
<p>Generalise <code>A || B</code> as follows:</p>
<ul>
<li>if <code>A</code> is <code>false</code> or <code>null</code>, evaluate and return <code>B</code></li>
<li>otherwise, return <code>A</code> without evaluating <code>B</code></li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; echo
echo

hi&gt; echo(&quot;Hello&quot;)
echo(&quot;Hello&quot;)

hi&gt; echo(&quot;Hello&quot;)!
Hello
null

hi&gt; &quot;Hello&quot;(0) || &quot;Z&quot;
&quot;H&quot;

hi&gt; &quot;Hello&quot;(99) || &quot;Z&quot;
&quot;Z&quot;

hi&gt; if(2 == 2, echo(&quot;OK&quot;)!, echo(&quot;WTF&quot;)!)
OK
null

hi&gt; true || echo(&quot;Don&#39;t do this&quot;)!
true

hi&gt; false &amp;&amp; echo(&quot;Don&#39;t do this&quot;)!
false

hi&gt; [# 00 ff #] &amp;&amp; echo(&quot;Just do it&quot;)!
Just do it
null</code></pre>
<h2 id="task-11-dictionaries">Task 11: Dictionaries</h2>
<ol type="1">
<li><p>Extend the data types in <code>HW3.Base</code> to include the following constructors:</p>
<pre><code>data HiFun =
  ...
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert

data HiValue =
  ...
  | HiValueDict (Map HiValue HiValue)

data HiExpr =
  ...
  | HiExprDict [(HiExpr, HiExpr)]</code></pre>
<p>Dictionaries are represented using the <code>Map</code> type from the <code>containers</code> package.</p></li>
<li><p>In the parser, add support for the following constructs:</p>
<ul>
<li><p>Built-in names <code>count</code>, <code>keys</code>, <code>values</code>, <code>invert</code>, that are parsed into the corresponding <code>HiFun</code> constructors.</p></li>
<li><p>Dictionary literals, written as <code>{ I: A, J: B, K: C }</code>, for example:</p>
<ul>
<li><code>{ "width": 120, "height": 80 }</code></li>
<li><code>{ 1: true, 3: true, 4: false }</code></li>
</ul></li>
<li><p>Dot access, written as <code>E.fld</code>, that is parsed into function application <code>E("fld")</code>. For example, the following holds:</p>
<pre><code>{ &quot;width&quot;: 120, &quot;height&quot;: 80 }.width
  ???
{ &quot;width&quot;: 120, &quot;height&quot;: 80 }(&quot;width&quot;)</code></pre></li>
</ul></li>
<li><p>In the evaluator, implement the new operations:</p>
<ul>
<li><code>{ "width": 120, "height": 80 }("width")</code> evaluates to <code>120</code></li>
<li><code>keys({ "width": 120, "height": 80 })</code> evaluates to <code>["height", "width"]</code> (sorted)</li>
<li><code>values({ "width": 120, "height": 80 })</code> evaluates to <code>[80, 120]</code> (sorted by key)</li>
<li><code>count("XXXOX")</code> evaluates to <code>{ "O": 1, "X": 4 }</code></li>
<li><code>count([# 58 58 58 4f 58 #])</code> evaluates to <code>{ 79: 1, 88: 4 }</code></li>
<li><code>count([true, true, false, true])</code> evaluates to <code>{ false: 1, true: 3 }</code></li>
<li><code>invert({ "x": 1, "y" : 2, "z": 1 })</code> evaluates to <code>{ 1: [ "z", "x" ], 2: ["y"] }</code></li>
</ul></li>
</ol>
<p>The following session in the REPL should be possible:</p>
<pre><code>hi&gt; count(&quot;Hello World&quot;).o
2

hi&gt; invert(count(&quot;big blue bag&quot;))
{ 1: [ &quot;u&quot;, &quot;l&quot;, &quot;i&quot;, &quot;e&quot;, &quot;a&quot; ], 2: [ &quot;g&quot;, &quot; &quot; ], 3: [&quot;b&quot;] }

hi&gt; fold(add, values(count(&quot;Hello, World!&quot;)))
13</code></pre></main>
  </body>
</html>
