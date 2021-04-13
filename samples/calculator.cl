(** calculator.cl *)

(** Utility math functions. *)
class Math {
  intMax : Int <- 2147483647;
  intMin : Int <- ~2147483647 - 1;

  (** COOL doesn't have mod so implement our own. *)
  mod(dividend : Int, divisor : Int) : Int {
    dividend - dividend / divisor * divisor
  };

  (** Good ol' Euclidean algorithm. Tail-recursive. *)
  gcd(x : Int, y : Int) : Int {
    if y = 0 then
      x
    else
      gcd(y, mod(x, y))
    fi
  };

  intMin() : Int {
    intMin
  };

  intMax() : Int {
    intMax
  };
};

class Number inherits IO {
  math : Math <- new Math;

  add(x : Number) : Number {
    x
  };

  addInteger(x : Integer) : Number {
    add(x)
  };

  addRational(x : Rational) : Number {
    add(x)
  };

  addBigInteger(x : BigInteger) : Number {
    add(x)
  };

  negate() : Number {
    self
  };

  sub(x : Number) : Number {
    add(x.negate())
  };

  print() : Object {
    0
  };
};

class Integer inherits Number {
  x : Int;

  init(y : Int) : SELF_TYPE {
    {
      x <- y;
      self;
    }
  };

  getValue() : Int {
    x
  };

  add(y : Number) : Number {
    y.addInteger(self)
  };

  addInteger(y : Integer) : Number {
    (new Integer).init(x + y.getValue())
  };

  negate() : Number {
    (new Integer).init(~x)
  };

  print() : Object {
    out_int(x)
  };
};

class Rational inherits Number {
  dividend : Int;
  divisor : Int;

  init(x : Int, y : Int) : SELF_TYPE {
    {
      x / y;
      if y < 0 then {
        init(~x, ~y);
      } else {
        let gcd : Int <- math.gcd(x, y) in {
          dividend <- x / gcd;
          divisor <- y / gcd;
          self;
        };
      } fi;
    }
  };

  getDividend() : Int {
    dividend
  };

  getDivisor() : Int {
    divisor
  };

  add(x : Number) : Number {
    x.addRational(self)
  };

  addInteger(x : Integer) : Number {
    (new Rational).init(dividend + x.getValue() * divisor, divisor)
  };

  addRational(x : Rational) : Number {
    let
      otherDividend : Int <- x.getDividend(),
      otherDivisor : Int <- x.getDivisor(),
      newDividend : Int <- dividend * otherDivisor + otherDividend * divisor,
      newDividsor : Int <- divisor * otherDivisor,
      gcd : Int <- math.gcd(newDividend, newDividsor)
    in
      (new Rational).init(newDividend / gcd, newDividsor / gcd)
  };

  negate() : Number {
    (new Rational).init(~dividend, divisor)
  };

  print() : Object {
    let
      gcd : Int <- math.gcd(dividend, divisor),
      x : Int <- dividend / gcd,
      y : Int <- divisor / gcd
    in {
      out_int(x);
      out_string("/");
      out_int(y);
    }
  };
};

class BigInteger inherits Number {
  max : Int <- 1000000000;
  x : Int;
  isNegative : Bool;
  next : BigInteger;

  init(y : Int) : SELF_TYPE {
    {
      if y = math.intMin() then {
        isNegative <- true;

      } else if y < 0 then {
        isNegative <- true;
        x <- ~y;
      } else {
        x <- y;
      } fi fi;
      self;
    }
  };

  initFrom(y : BigInteger, negative : Bool) : SELF_TYPE {
    {
      x <- y.head();
      next <- y.tail();
      isNegative <- negative;
      self;
    }
  };

  head() : Int {
    x
  };

  tail() : BigInteger {
    next
  };

  negate() : Number {
    (new BigInteger).initFrom(self, not isNegative)
  };

  print() : Object {
    {
      if not isvoid next then {
        next.print();
      } else {
        0;
      } fi;
      out_int(x);
    }
  };
};

class Main inherits IO {

  main(): Object {
    let
      x : Number <- (new Rational).init(847, 84),
      y : Number <- (new Integer).init(42)
    in {
      out_string("x = ");
      x.print();
      out_string("\ny = ");
      y.print();
      out_string("\nx + x = ");
      x.add(x).print();
      out_string("\ny + y = ");
      y.add(y).print();
      out_string("\nx + y = ");
      x.add(y).print();
      out_string("\ny + x = ");
      y.add(x).print();
      out_string("\n");
    }
  };
};
