package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  test(run("(26 + 6)"), "32")
  test(run("(((q) => q)(26) + ((k) => k)(6))"), "32")
  test(run("(26 * 6)"), "156")
  test(run("(((t) => t)(26) * ((l) => l)(6))"), "156")
  test(run("(((v) => v)(26) / ((m) => m)(6))"), "4")
  test(run("(26 / (-6))"), "-4")
  test(run("((-26) / 6)"), "-4")
  test(run("((-26) / (-6))"), "4")
  test(run("(((q) => q)(26) % ((l) => l)(6))"), "2")
  test(run("(26 % (-6))"), "2")
  test(run("((-26) % 6)"), "-2")
  test(run("((-26) % (-6))"), "-2")
  test(run("(((n) => n)(6) == ((p) => p)(6))"), "true")
  test(run("(6 == 26)"), "false")
  test(run("(((a) => a)(6) < ((o) => o)(6))"), "false")
  test(run("(6 < 26)"), "true")
  test(run("{ if (((m) => m)(true)) 6 else 26 }"), "6")
  test(run("{ if (true) 6 else x }"), "6")
  test(run("{ if (false) 6 else 26 }"), "26")
  test(run("(6, 26)"), "(6, 26)")
  test(run("((6, 26), (6, 26))"), "((6, 26), (6, 26))")
  test(run("(6, 26)._1"), "6")
  test(run("((x) => x)((6, 26))._1"), "6")
  test(run("(6, 26)._2"), "26")
  test(run("(6 :: Nil)"), "(6 :: Nil)")
  test(run("(26 :: ((t) => t)((6 :: Nil)))"), "(26 :: (6 :: Nil))")
  test(run("(Nil :: Nil)"), "(Nil :: Nil)")
  test(run("Nil.isEmpty"), "true")
  test(run("((d) => d)(Nil).isEmpty"), "true")
  test(run("(6 :: Nil).isEmpty"), "false")
  test(run("((i) => i)((6 :: Nil)).isEmpty"), "false")
  test(run("(6 :: Nil).head"), "6")
  test(run("((x) => x)((6 :: Nil)).head"), "6")
  test(run("(26 :: (6 :: Nil)).tail"), "(6 :: Nil)")
  test(run("((f) => f)((26 :: (6 :: Nil))).tail"), "(6 :: Nil)")
  test(run("{ val x = 6; x }"), "6")
  test(run("{ val x = true; { val x = 26; x } }"), "26")
  test(run("{ val x = 6; { val f = x; f } }"), "6")
  test(run("(() => 6)()"), "6")
  test(run("((x, f) => (x + f))(6, 26)"), "32")
  test(run("((x) => ((f) => (x + f)))(6)(26)"), "32")
  test(run("{ val x = 6; { def x() = x; x } }"), "<function>")
  test(run("{ val x = 6; { def x() = x; x() } }"), "<function>")
  test(run("{ val f = 6; { def x() = f; x() } }"), "6")
  test(run("{ def u(r) = { if ((r < 3)) (r + 5) else (u((r + (-1))) + (r + 5)) }; u(13) }"), "150")
  test(run("{ def h(d, n, t) = { if ((n < 1)) (t + 1) else (x((n + (-2))) + (d + 5)) }; def x(i) = { if ((i < 1)) (i + 2) else (h((i + 5), (i + (-2)), (i + 4)) + (i + 5)) }; x(12) }"), "95")
  test(run("{ def t(r, x) = { if ((x < 2)) (r + 4) else (q((r + 2), (x + (-1))) + (x + 2)) }; def k(j) = { if ((j < 3)) (j + 5) else (i((j + (-2))) + (j + 1)) }; def i(s) = { if ((s < 2)) (s + 3) else (l((s + (-2))) + (s + 5)) }; def l(o) = { if ((o < 1)) (o + 1) else (t((o + 1), (o + (-1))) + (o + 5)) }; def q(b, x) = { if ((x < 3)) (b + 3) else (k((x + (-1))) + (b + 2)) }; t(14, 11) }"), "86")
  test(run("(6.isInstanceOf[Int], false.isInstanceOf[Int], (6, 26).isInstanceOf[Int], Nil.isInstanceOf[Int], (6 :: Nil).isInstanceOf[Int], ((s) => s).isInstanceOf[Int])"), "(true, false, false, false, false, false)")
  test(run("(6.isInstanceOf[Boolean], false.isInstanceOf[Boolean], (6, 26).isInstanceOf[Boolean], Nil.isInstanceOf[Boolean], (6 :: Nil).isInstanceOf[Boolean], ((t) => t).isInstanceOf[Boolean])"), "(false, true, false, false, false, false)")
  test(run("(6.isInstanceOf[Tuple], false.isInstanceOf[Tuple], (6, 26).isInstanceOf[Tuple], Nil.isInstanceOf[Tuple], (6 :: Nil).isInstanceOf[Tuple], ((c) => c).isInstanceOf[Tuple])"), "(false, false, true, false, false, false)")
  test(run("(6.isInstanceOf[List], true.isInstanceOf[List], (6, 26).isInstanceOf[List], Nil.isInstanceOf[List], (6 :: Nil).isInstanceOf[List], ((s) => s).isInstanceOf[List])"), "(false, false, false, true, true, false)")
  test(run("(6.isInstanceOf[Function], false.isInstanceOf[Function], (6, 26).isInstanceOf[Function], Nil.isInstanceOf[Function], (6 :: Nil).isInstanceOf[Function], ((e) => e).isInstanceOf[Function])"), "(false, false, false, false, false, true)")
  test(run("(((z) => z)(6).isInstanceOf[Int], ((k) => k)(true).isInstanceOf[Int])"), "(true, false)")
  testExc(run("{ val x = 6; f }"), "")
  testExc(run("(6 + false)"), "")
  testExc(run("(6 * false)"), "")
  testExc(run("(6 / true)"), "")
  testExc(run("(6 / ((l) => l)(0))"), "")
  testExc(run("(6 % false)"), "")
  testExc(run("(6 % ((o) => o)(0))"), "")
  testExc(run("(true == false)"), "")
  testExc(run("(6 < false)"), "")
  testExc(run("{ if (6) 6 else 26 }"), "")
  testExc(run("(26 :: (6 :: Nil))._1"), "")
  testExc(run("(6, 26)._3"), "")
  testExc(run("(26 :: 6)"), "")
  testExc(run("(6, 26).isEmpty"), "")
  testExc(run("(6, 26).head"), "")
  testExc(run("(6, 26).tail"), "")
  testExc(run("((g) => g)(Nil).head"), "")
  testExc(run("((p) => p)(Nil).tail"), "")
  testExc(run("{ val f = (() => x); { val x = 6; f() } }"), "")
  testExc(run("6()"), "")
  testExc(run("((i) => i)()"), "")
  testExc(run("((f) => f)(6, 26)"), "")
  test(run("""
  def fill(a, n) =
    if (n == 0) Nil
    else a :: fill(a, n - 1);
  def map(l, f) =
    if (l.isEmpty) Nil
    else f(l.head) :: map(l.tail, f);
  def filter(l, p) =
    if (l.isEmpty) Nil
    else if (p(l.head)) l.head :: filter(l.tail, p)
    else filter(l.tail, p);
  def foldLeft(a, l, f) =
    def aux(i, r) =
      if (r.isEmpty) i
      else aux(f(i, r.head), r.tail);
    aux(a, l);
  val l = fill(1, 10);
  val l = foldLeft((6, Nil), l, (a, b) => (a._1 + b, a._1 + b :: a._2))._2;
  val l = filter(l, x => x % 2 != 0);
  val l = map(l, x => x * x);
  foldLeft(0, l, (a, b) => a + b)
  """), "645")
  test(run("""
  def isPrime(n) =
    def aux(m) =
      if (n == m) true
      else if (n % m == 0) false
      else aux(m + 1);
    aux(2);
  def factorize(n) =
    if (isPrime(n)) n :: Nil
    else
      def aux(m) =
        if (n % m == 0) m :: factorize(n / m)
        else aux(m + 1);
      aux(2);
  factorize(936)
  """), "(2 :: (2 :: (2 :: (3 :: (3 :: (13 :: Nil))))))")
  test(run("""
  def interp(e) =
    if (e.isInstanceOf[Int]) e
    else if (e.head) interp_plus(e.tail)
    else interp_minus(e.tail);
  def interp_plus(e) =
    val v1 = interp(e.head);
    val v2 = interp(e.tail.head);
    v1 + v2;
  def interp_minus(e) =
    val v1 = interp(e.head);
    val v2 = interp(e.tail.head);
    v1 - v2;
  interp(true :: (false :: 6 :: 26 :: Nil) :: 6 :: Nil)
  """), "-14")
  test(run("""
  def merge(l, r) =
    if (l.isEmpty) r
    else if (r.isEmpty) l
    else
      val x = l.head;
      val y = r.head;
      if (x <= y) x :: merge(l.tail, r)
      else y :: merge(l, r.tail);
  def split(o) =
    if (o.isEmpty) (Nil, Nil)
    else if (o.tail.isEmpty) (o, Nil)
    else
      val x = o.head;
      val y = o.tail.head;
      val zs = o.tail.tail;
      val (xs, ys) = split(zs);
      (x :: xs, y :: ys);
  def mergeSort(o) =
    if (o.isEmpty) Nil
    else if (o.tail.isEmpty) o
    else
      val (as, bs) = split(o);
      merge(mergeSort(as), mergeSort(bs));
  mergeSort(9 :: 10 :: 8 :: 7 :: 6 :: Nil)
  """), "(6 :: (7 :: (8 :: (9 :: (10 :: Nil)))))")
  test(run("""
  val emptyTree = 0;
  def makeNode(x, l, r) = (x, l, r);
  def match(tree) = (f, g) =>
    if (tree.isInstanceOf[Tuple])
      val (x, l, r) = tree;
      g(x, l, r)
    else f();
  def add(tree, elem) = match(tree)(
    () => makeNode(elem, emptyTree, emptyTree),
    (x, l, r) =>
      if (elem < x) makeNode(x, add(l, elem), r)
      else if (elem > x) makeNode(x, l, add(r, elem))
      else makeNode(x, l, r)
  );
  def append(l1, l2) =
    if (l1.isEmpty) l2
    else l1.head :: append(l1.tail, l2);
  def flatten(tree) = match(tree)(
    () => Nil,
    (x, l, r) => append(flatten(l), x :: flatten(r))
  );
  val t = emptyTree;
  val t = add(t, 12);
  val t = add(t, 24);
  val t = add(t, 18);
  val t = add(t, 6);
  val t = add(t, 30);
  flatten(t)
  """), "(6 :: (12 :: (18 :: (24 :: (30 :: Nil)))))")

  /* Write your own tests */
  // 0 difivision check
  testExc(run("10 / 0"), "")
  testExc(run("10 % 0"), "")
  test(run("""
    def makeAdder(n) = (x) => x + n;
    def map(l, f) =
      if (l.isEmpty) Nil
      else f(l.head) :: map(l.tail, f);
    val add7 = makeAdder(7);
    val list = 10 :: 20 :: 30 :: Nil;
    map(list, add7)
  """), "(17 :: (27 :: (37 :: Nil)))")

  // 테스트 2: 상호 재귀를 이용한 리스트 분석 (Mutual Recursion on Lists)
  // isEven과 isOdd 함수는 서로를 호출하며 리스트 길이의 홀/짝 여부를 판단합니다.
  // Nil에 도달했을 때의 베이스 케이스를 통해 재귀가 종료됩니다.
  test(run("""
    def isEven(l) =
      if (l.isEmpty) true
      else isOdd(l.tail);
    def isOdd(l) =
      if (l.isEmpty) false
      else isEven(l.tail);
    val list1 = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
    val list2 = 1 :: 2 :: 3 :: 4 :: Nil;
    (isOdd(list1), isEven(list2))
  """), "(true, true)")

  // 테스트 3: 복잡한 튜플 구조 분해 및 활용 (Complex Tuple Destructuring)
  // 함수 반환값인 튜플을 val 패턴 매칭을 통해 여러 변수에 한 번에 할당하고,
  // 이 변수들을 복잡한 수식에 사용하여 최종 결과를 계산합니다.
  test(run("""
    def calculator(a, b) = (a + b, a * b, if (a > b) a else b);
    val (sum, prod, max) = calculator(7, 12);
    prod - sum - max
  """), "53")

  // 테스트 4: isInstanceOf를 사용한 동적 타입 분기 (Dynamic Type Dispatch with isInstanceOf)
  // 다양한 타입의 값을 가진 리스트를 순회하며, isInstanceOf를 사용해 타입을 확인하고
  // 각 타입에 맞는 연산을 수행하여 합계를 구합니다. (함수는 100, 튜플은 첫 요소, 리스트는 길이)
  test(run("""
    def sumMixedList(l) =
      if (l.isEmpty) 0
      else
        val head = l.head;
        val term =
          if (head.isInstanceOf[Int]) head
          else if (head.isInstanceOf[Boolean]) if (head) 1 else 0
          else if (head.isInstanceOf[Tuple]) head._1
          else if (head.isInstanceOf[List]) if (head.isEmpty) 0 else head.head
          else if (head.isInstanceOf[Function]) 100
          else 0;
        term + sumMixedList(l.tail);

    val mixed = 10 :: (25, 5) :: (3 :: Nil) :: true :: ((x)=>x) :: Nil;
    sumMixedList(mixed)
  """), "139")

  // 테스트 5: 함수와 튜플을 이용한 '객체' 상태 시뮬레이션 (State Simulation with Tuples and Functions)
  // 'inc' 또는 'dec' 함수는 현재 상태(값)를 기반으로 새로운 상태를 가진 '객체'(튜플)를 반환합니다.
  // 이를 통해 불변성을 유지하면서 상태 변화를 흉내냅니다.
  test(run("""
    def makeCounter(v) = (
      () => v,
      () => makeCounter(v + 1),
      () => makeCounter(v - 1)
    );
    val get = (c) => c._1();
    val inc = (c) => c._2();
    val dec = (c) => c._3();

    val c0 = makeCounter(10);
    val c1 = inc(c0);
    val c2 = inc(c1);
    val c3 = dec(c2);
    get(c3)
  """), "11")

  // 테스트 6: Desugaring 규칙에 의존하는 복합 논리 연산 (Complex Boolean Logic relying on Desugaring)
  // >=, <=, !=, &&, || 연산자가 복합적으로 사용된 표현식입니다.
  // 이는 명세서의 Desugaring 규칙에 따라 더 기본적인 연산( <, ==, if-else )으로 변환되어 평가됩니다.
  test(run("""
    val a = 10;
    val b = 20;
    val c = 10;
    (a <= b) && (b >= a) && (a != b) && !(a == c) || (a < c)
  """), "false")

  // 테스트 7: 중첩된 자료구조 재귀 탐색 (Recursive Traversal of Nested Data Structures)
  // (Int, List) 형태의 튜플을 요소로 가지는 리스트를 재귀적으로 탐색하며,
  // 모든 정수 값의 합을 계산합니다. 외부 리스트와 내부 리스트를 모두 순회해야 합니다.
  test(run("""
    def sumList(l) = if (l.isEmpty) 0 else l.head + sumList(l.tail);
    def sumNested(l) =
      if (l.isEmpty) 0
      else
        val item = l.head;
        item._1 + sumList(item._2) + sumNested(l.tail);

    val nestedList = (5, (10 :: 20 :: Nil)) :: (15, (25 :: 30 :: Nil)) :: Nil;
    sumNested(nestedList)
  """), "105")

  // 테스트 8: 함수 합성 (Function Composition)
  // 두 함수 f와 g를 받아 f(g(x))를 수행하는 새로운 함수를 반환하는 compose 고차 함수를 구현하고 테스트합니다.
  // 이는 함수가 일급 시민임을 보여주는 핵심적인 예제입니다.
  test(run("""
    def compose(f, g) = (x) => f(g(x));
    def square(x) = x * x;
    def inc(x) = x + 1;

    val incThenSquare = compose(square, inc);
    val squareThenInc = compose(inc, square);

    (incThenSquare(5), squareThenInc(5))
  """), "(36, 26)")

  // 테스트 9: 연관 리스트를 이용한 피보나치 메모이제이션 (Fibonacci with Memoization using Association List)
  // 재귀 호출 시 계산된 값을 (key, value) 형태의 튜플 리스트(연관 리스트)에 저장하여
  // 중복 계산을 피합니다. 이는 복잡한 재귀, 리스트 처리, 튜플 조작을 모두 시험합니다.
  test(run("""
    def find(key, memo) =
      if (memo.isEmpty) -1
      else if (memo.head._1 == key) memo.head._2
      else find(key, memo.tail);

    def fib(n, memo) =
      if (find(n, memo) != -1) (find(n, memo), memo)
      else if (n < 2) (n, (n, n) :: memo)
      else
        val (res1, memo1) = fib(n - 1, memo);
        val (res2, memo2) = fib(n - 2, memo1);
        val result = res1 + res2;
        (result, (n, result) :: memo2);

    fib(12, Nil)._1
  """), "144")

  // 테스트 10: 잘못된 타입에 대한 연산 시도 (런타임 오류)
  // 리스트에만 사용 가능한 .head 연산을 튜플에 적용하여, 명세서에 정의된 대로
  // 런타임 오류가 정상적으로 발생하는지 확인하는 예외 테스트입니다.
  testExc(run("""
    def getHead(collection) = collection.head;
    val myTuple = (1, 2, 3);
    getHead(myTuple)
  """), "")
  /*
   작성자: Gemini
   극한 상황 및 복합 기능 심층 테스트 10개
  */

  // 테스트 11: 어휘적 범위(Lexical Scope)와 변수 섀도잉(Shadowing)
  // 클로저가 생성될 때 캡처된 변수 'x' (값: 100)가,
  // 해당 클로저가 실행되는 외부 환경에 선언된 또 다른 'x' (값: 1)에 의해 영향을 받지 않음을 증명합니다.
  // 이는 FIBER가 어휘적(정적) 범위를 정확히 따르는지 확인합니다.
  test(run("""
    val x = 100;
    def f() = x;
    def g(func) = {
      val x = 1;
      func()
    };
    g(f)
  """), "100")

  // 테스트 12: 깊은 재귀와 함수 호출 스택 (Ackermann Function)
  // 극도로 빠른 속도로 성장하는 아커만 함수를 통해 깊은 재귀 호출을 테스트합니다.
  // 이 테스트는 함수 호출 스택의 한계와 재귀적 함수 처리의 효율성을 시험합니다.
  test(run("""
    def ack(m, n) =
      if (m == 0) n + 1
      else if (n == 0) ack(m - 1, 1)
      else ack(m - 1, ack(m, n - 1));
    ack(3, 2)
  """), "29")

  // 테스트 13: 연산자 우선순위와 결합 규칙 혼합
  // `*`, `+`, `<`, `&&`, `::` 등 다양한 우선순위와 결합 규칙(좌/우)을 가진 연산자들을
  // 하나의 표현식에 혼합하여 파서와 평가기가 명세서대로 정확히 동작하는지 검증합니다.
  // `+`가 `::`보다 우선순위가 높으므로 (1+5) :: (2*4) :: Nil 로 평가되어야 합니다.
  test(run("""
    val result = 1 + 5 :: 2 * 4 :: Nil;
    val check = 10 < 20 && result.tail.head == 8;
    (result, check)
  """), "((6 :: (8 :: Nil)), true)")

  // 테스트 14: 함수 리스트에 대한 고차 함수 적용
  // 함수 자체를 요소로 가지는 리스트를 만들고, 이 리스트를 순회하며 각 함수를
  // 이전 함수의 결과값에 연속적으로 적용합니다. (f3(f2(f1(x))))
  test(run("""
    def applyAll(funcs, v) =
      if (funcs.isEmpty) v
      else applyAll(funcs.tail, funcs.head(v));

    val pipeline = ((x) => x * x) :: ((x) => x + 10) :: ((x) => x / 2) :: Nil;
    applyAll(pipeline, 10)
  """), "55")


  // 테스트 16: 튜플과 고차 함수를 이용한 '지연 평가 스트림' 시뮬레이션
  // (값, 다음 값을 생성하는 함수) 형태의 튜플을 재귀적으로 생성하여 무한 스트림을 흉내냅니다.
  // 'get' 함수는 스트림을 필요한 만큼만 평가하여 n번째 원소를 찾아냅니다.
  test(run("""
    def naturals(n) = (n, () => naturals(n + 1));
    def get(s, n) =
      if (n == 0) s._1
      else get(s._2(), n - 1);
    val nat_stream = naturals(0);
    get(nat_stream, 15)
  """), "15")

  // 테스트 17: 중첩된 isInstanceOf를 이용한 복잡 타입 분석
  // List -> Tuple -> (Boolean | Int) 형태로 중첩된 값의 타입을 isInstanceOf로 재귀적으로 분석하여


  // 테스트 18: 이진 탐색 트리(BST)의 깊이 계산
  // 명세서 예제에 나온 이진 트리를 활용하여, 가장 깊은 노드까지의 경로를 찾는 재귀 함수를 테스트합니다.
  // 이는 복잡한 재귀 로직과 튜플 기반 자료구조 처리를 검증합니다.

  // 테스트 20: 빈 리스트에 대한 집계 함수 호출 시 발생하는 예외
  // 원소가 없는 빈 리스트에서 최솟값을 찾으려고 할 때, `.head` 연산을 시도하여
  // 의도된 대로 런타임 오류가 발생하는지 확인하는 예외 테스트입니다.
  testExc(run("""
    def findMin(l) = {
      def aux(min, rest) =
        if (rest.isEmpty) min
        else if (rest.head < min) aux(rest.head, rest.tail)
        else aux(min, rest.tail);
      aux(l.head, l.tail)
    };
    findMin(Nil)
  """), "")
}
