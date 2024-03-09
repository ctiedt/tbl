# Task Based Language

What if all your functions were cyclical tasks? The *Task Based Language* (TBL) is an attempt
to create a programming language that captures the essentials of many realtime applications
by putting thís idea into the foreground.

TBL is still work in progress. While an example scheduler exists, many features are still
missing or only partially implemented. Use at your own risk!

## Example

```
extern task printf(...);    // (1)

global n: u32 = 0;          // (2)

task count(counter: &u32) { // (3)
    *counter = counter + 1;
    printf("%d\n", *counter);
    return;
}

task main() -> u32 {
    schedule count(&n);      // (4)
    return 0;
}
```

This example doesn't run yet, but this is the "Hello World" I'm working towards.
Comment (1) shows how easily you can integrate functions with a C ABI, e.g. a regular
libc function. For our example, we'll use a global value (see comment (2)) as our
task state. Comment (3) shows how you declare a task. TBL doesn't differentiate
between tasks and functions: even external functions can be used as tasks! Finally,
comment (4) shows how you can schedule a task. The default scheduler will be started
at the end of your main function, but you can also replace it with your own implementation.

If you want to look at some examples that do run, check out the [examples](examples)
folder. You can also read the [documentation](docs).