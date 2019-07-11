#include "opt.h"

#ifdef HAVE_WINDOWS_H
/*
#include <HtmlHelp.h>
*/

DEF_SUBCMD(cmd_man) {
  /*
    char *file = ;
    char *keyword = ;
    HH_AKLINK link = {sizeof link};
    link.pszKeywords = keyword;
    link.fIndexOnFail = 1;

    if (HtmlHelp (GetDesktopWindow (), file, HH_KEYWORD_LOOKUP, (DWORD)&link))
    return 0;

    HH_LAST_ERROR err = {sizeof err};
    if (HtmlHelp (0, 0, HH_GET_LAST_ERROR, (DWORD)&err)
    && FAILED (err.hr)) {
    if (err.description) {
    USES_CONVERSION;
    char *desc = W2A (err.description);
    SysFreeString (err.description);
    return 2;
    }
    else {
    return 3;
    }
    }
  */
  return 0;
}

#endif
