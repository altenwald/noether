![](butterfly.jpg) noether
==========================

[**WIP**] Noether language on BEAM.

Why
---

The idea of Noether language is to provide to the BEAM platform of a C/C++/Java-friendly language to do easier the adoption of the BEAM.

The language has the most of the Java syntax (is hardly influenced by it) but with more additions from other languages.

The strengths of the language will be:

- High concurrency support (millions of threads).
- Soft-realtime.
- Multi-node.
- Hot-swap of the code.
- Communication between threads.
- Compilation to BEAM directly.
- Integration with Erlang/Elixir/...

The name
--------

In the same way like Ada or Julia I wanted to give more presence to the women work in science. I was looking for the perfect one and finally I found to [Emmy Noether][1] who ennunced the symmetry law (that's because the logo is a butterfly).

The syntax
----------

In a nutshell...

```java
package com.data;

import org.noether.Integer;
import org.noether.util.*;

public class Whatever extends Other implements Else, More {
    public int i = 10;
    public static String message = "Hello world!";
    public static final int max = 100;

    public Whatever() {
        this.i = 10;
    }

    public int data(int i, int j) {
        return i + j;
    }

    public static int main(String[] args) {
        Whatever w = new Whatever();
        int i, j = 0;
        for (i=0; i<10; i++) {
            System.out.println(w.data(10, 20));
            j++;
        }
        return 0;
    }
}
```

[1]: https://en.wikipedia.org/wiki/Emmy_Noether
