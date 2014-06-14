# What's implemented?

* The core of the language

* Objects and basic operations

* Object searching operations

# Examples
## Core language

    {
      val x = alloc(1);

      if (*x == 1) {
        print("x is one");
        println()
      };

      while (*x < 10) {
        print(*x);
        println();
        x = *x + 1
      };

      if (not (*x == 10)) {
         print("x is not ten");
         println()
      } else {
        print("x is ten");
        println()
      };

      free(x);
      return 1;
    }

Passing functions as arguments,

    {
      val x = alloc(1);

      function g(f : fun(int,int)) {
        print("");
        return f(*x + 3);
      }

      while (*x < 10) {
        print(g(function (y: int) { print(""); return y+1; }));
        println();
        x = *x + 1
      };

      free(x);
      return 1;

    }

## Basic objects

    {
      val x = {
        a : 1,
        b: {
          as: [
            { n: "0", v: 0 },
            { n: "1", v: 1 },
            { n: "2", v: 2 }
          ],
          bs: [1,2,3,4]
        }
      };

      for (v : x.b.bs) {
        print(x.b.as[0].v + v);
        println();
        return 1;
      };

      return 1;
    }

## Object filtering

    {
      val x = {
        a : 1,
        b: {
          as: [
            { n: "0", v: 0 },
            { n: "1", v: 1 },
            { n: "2", v: 2 }
          ],
          bs: [1,2,3,4]
        }
      };

      for (u : x.b.as, u.v > 0) {
        print(u.n);
        println();
        return 1;
      };

      return 1;
    }
