// Stubs for EventRacer calls, for use with D8.

// Core functions
global._ER_read = function(name, value) {
    if ("ER_read" in global)
        return global.ER_read(name, value);
    else
        return value;
}

global._ER_readProp = function(obj, name, value) {
    if ("ER_readProp" in global)
        return global.ER_readProp(obj, name, value);
    else
        return value;
}

global._ER_write = function(name, value) {
    if ("ER_write" in global)
        return global.ER_write(name, value);
    else
        return value;
}

global._ER_writeProp = function(obj, name, value) {
    if ("ER_writeProp" in global)
        return global.ER_writeProp(obj, name, value);
    else
        return value;
}

global._ER_delete = function(name) {
    if ("ER_delete" in global)
        global.ER_delete(name);
}

global._ER_deleteProp = function(obj, name) {
    if ("ER_deleteProp" in global)
        global.ER_deleteProp(obj, name);
}

// Helper functions
global.ER_readPropIdx = function(arr, idx) {
    return global._ER_readProp(arr, idx, arr[idx]);
}

global.ER_writePropIdx = function(obj, idx, value) {
    return obj[idx] = global._ER_writeProp(obj, idx, value);
}

global.ER_writePropIdxStrict = function(obj, idx, value) {
    "use strict";
    return obj[idx] = global._ER_writeProp(obj, idx, value);
}

global.ER_preIncProp = function(obj, idx) {
    return obj[idx] = global._ER_writeProp(obj, idx, obj[idx] + 1);
}

global.ER_preIncPropStrict = function(obj, idx) {
    "use strict";
    return obj[idx] = global._ER_writeProp(obj, idx, obj[idx] + 1);
}

global.ER_preDecProp = function(obj, idx) {
    return obj[idx] = global._ER_writeProp(obj, idx, obj[idx] - 1);
}

global.ER_preDecPropStrict = function(obj, idx) {
    "use strict";
    return obj[idx] = global._ER_writeProp(obj, idx, obj[idx] - 1);
}

global.ER_postIncProp = function(obj, idx) {
    var tmp = obj[idx];
    obj[idx] = global._ER_writeProp(obj, idx, tmp + 1);
    return tmp;
}

global.ER_postIncPropStrict = function(obj, idx) {
    "use strict";
    var tmp = obj[idx];
    obj[idx] = global._ER_writeProp(obj, idx, tmp + 1);
    return tmp;
}

global.ER_postDecProp = function(obj, idx) {
    var tmp = obj[idx];
    obj[idx] = global._ER_writeProp(obj, idx, tmp - 1);
    return tmp;
}

global.ER_postDecPropStrict = function(obj, idx) {
    "use strict";
    var tmp = obj[idx];
    obj[idx] = global._ER_writeProp(obj, idx, tmp - 1);
    return tmp;
}

global.ER_deletePropIdx = function(obj, idx) {
    global._ER_deleteProp(obj, idx);
    return delete obj[idx];
}

global.ER_deletePropIdxStrict = function(obj, idx) {
    "use strict";
    global._ER_deleteProp(obj, idx);
    return delete obj[idx];
}
