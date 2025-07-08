#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define _GNU_SOURCE
#define __USE_GNU
#include <sys/time.h>

#include "tcb.h"

typedef void *(task_t)(void *, void *);
typedef void(handler_t)(void *, void *);

typedef struct {
  task_t *task;
  void *args;
  handler_t *handlers[64];
  int n_handlers;
  struct timeval period;
  tcb_t tcb;
} task_state;

task_state TASKS[1024];
uint64_t TASK_IDX = 0;

void print_tcb(tcb_t *tcb) {
  printf("tcb = { .copy_locals = %d, .once_mask = %ld, .locals = %p }\n",
         tcb->copy_locals, tcb->once_mask, tcb->locals);
}

void task_sleep(uint64_t id) {
  if (TASKS[id].tcb.state != TASK_EXITED) {
    TASKS[id].tcb.state = TASK_WAITING;
  }
}

void *thread_runner(void *arg) {
  uint64_t task_id = (uint64_t)arg;
  task_state task = TASKS[task_id];
  while (1) {
    struct timeval start;
    gettimeofday(&start, NULL);
    void *res = (task.task)(task.args, &task.tcb);
    for (int i = 0; i < task.n_handlers; i++) {
      handler_t *handler = task.handlers[i];
      (handler)(res, 0);
    }
    task_sleep(task_id);
    struct timeval now;
    gettimeofday(&now, NULL);
    struct timeval elapsed;
    timersub(&now, &start, &elapsed);
    struct timeval to_sleep_tv;
    timersub(&task.period, &elapsed, &to_sleep_tv);
    struct timespec to_sleep;
    TIMEVAL_TO_TIMESPEC(&to_sleep_tv, &to_sleep);
    nanosleep(&to_sleep, NULL);
  }
}

int sched_enqueue(task_t task, void *args, int64_t arg_size, int64_t period) {
  task_state state;
  state.task = task;
  state.args = malloc(arg_size);
  state.n_handlers = 0;
  state.tcb.id = TASK_IDX;
  state.tcb.copy_locals = 0;
  state.tcb.state = TASK_INIT;
  state.tcb.once_mask = 0;
  state.tcb.locals = malloc(1024);
  memcpy(state.args, args, arg_size);

  int period_secs = period / 1000;
  int period_micros = (period % 1000) * 1000;

  state.period.tv_sec = period_secs;
  state.period.tv_usec = period_micros;

  TASKS[TASK_IDX] = state;
  TASK_IDX++;

  pthread_t thread;
  pthread_create(&thread, NULL, thread_runner, (void *)(TASK_IDX - 1));

  return TASK_IDX - 1;
}

void sched_exit(uint64_t task_id) { TASKS[task_id].tcb.state = TASK_EXITED; }

void sched_attach(int task_handle, handler_t handler) {
  task_state *state = &TASKS[task_handle];
  state->handlers[state->n_handlers] = handler;
  state->n_handlers++;
}

void sched_run() {
  while (1) {
    int any_running = 0;
    for (int i = 0; i < TASK_IDX; i++) {
      if (TASKS[i].tcb.state != TASK_EXITED) {
        any_running = 1;
      }
    }
    if (!any_running) {
      exit(0);
    }
  }
}
