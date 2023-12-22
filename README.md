# Task Based Language

What if all your functions were cyclical tasks? The *Task Based Language* (TBL) is an attempt
to create a programming language that captures the essentials of many realtime applications
by putting thís idea into the foreground.

This language is far from done (mainly the scheduler does not work yet and there are probably
hundreds of bugs I haven't caught yet), but the basics are usable.

## Example

```
extern task printf(...);    // (1)

task count(counter: &u32) { // (2)
    *counter = counter + 1;
    printf("%d\n", *counter);
    return;
}

task main() -> u32 {
    schedule count(0);      // (3)
    return 0;
}
```

This example doesn't run yet, but this is the "Hello World" I'm working towards.
Comment (1) shows how easily you can integrate functions with a C ABI, e.g. a regular
libc function. Comment (2) shows how you declare a task. TBL doesn't differentiate
between tasks and functions: even external functions can be used as tasks! Finally,
comment (3) shows how you can schedule a task. The default scheduler will be started
before your main function, but you can also replace it with your own implementation.

If you want to look at some examples that do run, check out the [examples](examples)
folder. You can also read the [documentation](docs).