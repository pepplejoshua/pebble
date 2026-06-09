#ifndef PROLE_VALIDATE_H
#define PROLE_VALIDATE_H

#include "prole_bytecode.h"
#include "prole_diag.h"
#include <stdbool.h>

bool prole_validate_module(const ProleModule *module,
                           ProleDiagnosticContext *diagnostics);
bool prole_validate_runnable_module(const ProleModule *module,
                                    ProleDiagnosticContext *diagnostics);

#endif
