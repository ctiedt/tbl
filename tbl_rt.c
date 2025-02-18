#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef void *(task_t)(void *);
typedef void (handler_t)(void*);

typedef struct {
  task_t *task;
  void *args;
  handler_t *handlers[64];
  int n_handlers;
} task_state;

task_state TASKS[1024];
int TASK_IDX = 0;

void handle_sigev(int sig, siginfo_t *info, void *ctx) {
  int task_id = info->si_value.sival_int;
  // printf("%d", task_id);
  task_state state = TASKS[task_id];
  void *res = (state.task)(state.args);
  for (int i = 0; i < state.n_handlers; i++) {
    handler_t *handler = state.handlers[i];
    (handler)(res);
  }
}

int sched_enqueue(task_t task, void *args, int64_t arg_size, int64_t period) {
  task_state state;
  state.task = task;
  state.args = malloc(arg_size);
  state.n_handlers = 0;
  memcpy(state.args, args, arg_size);

  TASKS[TASK_IDX] = state;
  
  timer_t timer;
  sigevent_t sev;
  sev.sigev_notify = SIGEV_SIGNAL;
  sev.sigev_signo = SIGRTMIN;
  sev.sigev_value.sival_int = TASK_IDX;
  if (timer_create(CLOCK_REALTIME, &sev, &timer) == -1) {
    // perror("timer_create");
    exit(-1);
  }
  TASK_IDX++;

  int period_secs = period / 1000;
  int period_nanos = (period % 1000) * 1000000;
  
  struct itimerspec its;
  its.it_value.tv_sec = period_secs;
  its.it_value.tv_nsec = period_nanos;
  its.it_interval.tv_sec = period_secs;
  its.it_interval.tv_nsec = period_nanos;
  if (timer_settime(timer, 0, &its, NULL) == -1) {
    // perror("timer_settime");
    exit(-1);
  }
  return TASK_IDX - 1;
}


void sched_attach(int task_handle, handler_t handler) {
  task_state *state = &TASKS[task_handle];
  state->handlers[state->n_handlers] = handler;
  state->n_handlers++;
}

void sched_run() {
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = handle_sigev;
  if (sigaction(SIGRTMIN, &sa, NULL) == -1) {
    // perror("sigaction");
    exit(-1);
  }

  while (1)
    ;
}
