# -*- python -*-
#
# Settings for interactive numpy sessions.  Add to .python like this:
#    execfile("~/.numpy")
#

if mod_available("numpy"):
    import numpy as np

def aseq(dims):
    arr = np.empty(dims, dtype=int)
    arr.flat = xrange(arr.size)
    return arr
