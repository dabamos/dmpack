#include "ffc.h"

#include <stddef.h>
#include <stdint.h>

/* Match Fortran's PRESET_GENERAL which includes whitespace skipping */
static ffc_parse_options ffc_bench_options(void) {
    ffc_parse_options opts;
    opts.format = FFC_PRESET_GENERAL | FFC_FORMAT_FLAG_SKIP_WHITE_SPACE;
    opts.decimal_point = '.';
    return opts;
}

/* Per-line parse with whitespace skipping (matches Fortran DEFAULT_PARSING) */
ffc_result ffc_parse_double_ws(size_t len, const char *s, double *out) {
    char *pend = (char*)(s + len);
    ffc_parse_options opts = ffc_bench_options();
    return ffc_from_chars_double_options(s, pend, out, opts);
}

double benchmark_ffc_lines(const char *data, const size_t *offsets, const size_t *lengths, size_t nlines, uint32_t *outcome) {
    double answer;
    double value;
    size_t i;
    ffc_parse_options opts = ffc_bench_options();

    answer = 0.0;
    value = 0.0;
    *outcome = FFC_OUTCOME_OK;

    for (i = 0; i < nlines; ++i) {
        ffc_result res;

        res = ffc_from_chars_double_options(data + offsets[i], data + offsets[i] + lengths[i], &value, opts);
        if (res.outcome != FFC_OUTCOME_OK) {
            *outcome = res.outcome;
            continue;
        }
        answer = answer > value ? answer : value;
    }

    return answer;
}
