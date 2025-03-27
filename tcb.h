#include <stdint.h>

typedef enum {
  TASK_INIT,
  TASK_RUNNING,
  TASK_WAITING,
  TASK_EXITED
} task_state_t;

typedef struct {
  uint64_t id;
  uint8_t copy_locals;
  task_state_t state;
  uint64_t once_mask;
  void *locals;
} tcb_t;
