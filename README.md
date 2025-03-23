# Task Based Language

What if all your functions were cyclical tasks? The _Task Based Language_ (TBL)
is an attempt to create a programming language that captures the essentials of
many realtime applications by putting thís idea into the foreground.

TBL is still work in progress. While an example scheduler exists, many features
are still missing or only partially implemented. Use at your own risk and report
compiler bugs if you find any!

## Example

```
use std;

task print(v: u32) {
	printf("%d\n", v);
}

task count(from: u32) -> u32 {
	uninit n: u32;
	once {
		n = from;
	}
	n = n + 1;
	return n;
}

task main() {
	let t: handle = schedule count(41) every 100ms;
	on t do print;
}
```

The example above implements a simple counter. Let's start with the main task:
As you see, we dont't just call `count`, we `schedule` it. This means `count`
will be executed every 100ms. Jumping to the definition of `count`, you may
notice the `uninit` syntax and `once` block. These are necessary because
scheduled tasks save the state of their local variables every time they run.
Putting the initialization of `n` into a `once` block ensures that it is only
initialized once and the counter will count up as intended. Returning to the
main task, you can see that it is also possible to attach tasks to other tasks:
Every time `count` runs, `print` will be called with its return value and
thereby print the current value of `n`.

If you want to look at some examples that do run, check out the
[examples](examples) folder. You can also read the [documentation](docs).
