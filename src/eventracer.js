// Operations encoding.
const ER_OP_read          =  1;
const ER_OP_readProp      =  2;
const ER_OP_readArray     =  3;
const ER_OP_write         =  4;
const ER_OP_writeProp     =  5;
const ER_OP_writeFunc     =  6;
const ER_OP_writePropFunc =  7;
const ER_OP_writeArray    =  8;
const ER_OP_enterFunc     =  9;
const ER_OP_exitFunc      = 10;
const ER_OP_delete        = 11;
const ER_OP_deleteProp    = 12;

// Operations
global.ER_op = [];

// Accessed objects.
global.ER_obj = [];

// Property or function names.
global.ER_name = [];

// Read/written values; integer function id for enterFunc.
global.ER_value = [];

// Script identifiers
global.ER_scriptId = [];

// Function identifiers.
global.ER_funcId = [];

// Function start and end line number.
global.ER_startLine = [];
global.ER_endLine = [];

// Collection enable counter.
global.ER_enableCount = 0;

// Clears the data and increments the collection enable counter.
global.ER_enable = function() {
    if (++global.ER_enableCount == 1) {
        global.ER_op.length = 0;
        global.ER_obj.length = 0;
        global.ER_name.length = 0;
        global.ER_value.length = 0;
        global.ER_scriptId.length = 0;
        global.ER_funcId.length = 0;
        global.ER_startLine.length = 0;
        global.ER_endLine.length = 0;
    }
}

// Decrements the enable counter. Data is not collected if the counter
// is zero.
global.ER_disable = function() {
    return --global.ER_enableCount;
}

// Core functions
function ER_read(name, value) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_read);
        global.ER_name.push_(name);
        global.ER_value.push_(value);
    }
    return value;
}

function ER_readProp(obj, name, value) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_readProp);
        global.ER_obj.push_(obj);
        global.ER_name.push_(name);
        global.ER_value.push_(value);
    }
    return value;
}

function ER_readArray(array) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_readArray);
        global.ER_obj.push_(array);
    }
}

function ER_write(name, value) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_write);
        global.ER_name.push_(name);
        global.ER_value.push_(value);
    }
    return value;
}

function ER_writeProp(obj, name, value) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_writeProp);
        global.ER_obj.push_(obj);
        global.ER_name.push_(name);
        global.ER_value.push_(value);
    }
    return value;
}

function ER_writeFunc(name, value, id) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_writeFunc);
        global.ER_name.push_(name);
        global.ER_funcId.push_(id);
    }
    return value;
}

function ER_writePropFunc(obj, name, value, id) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_writePropFunc);
        global.ER_obj.push_(obj);
        global.ER_name.push_(name);
        global.ER_funcId.push_(id);
    }
    return value;
}

function ER_writeArray(array) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_writeArray);
        global.ER_obj.push_(array);
    }
}


function ER_enterFunction(name, scriptId, fnId, startLine, endLine) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_enterFunc);
        global.ER_name.push_(name);
        global.ER_scriptId.push_(scriptId);
        global.ER_funcId.push_(fnId);
        global.ER_startLine.push_(startLine);
        global.ER_endLine.push_(endLine);
    }
}

function ER_exitFunction(val) {
    if (global.ER_enableCount)
        global.ER_op.push_(ER_OP_exitFunc);
    return val;
}

function ER_delete(name) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_delete);
        global.ER_name.push_(name);
    }
}

function ER_deleteProp(obj, name) {
    if (global.ER_enableCount) {
        global.ER_op.push_(ER_OP_deleteProp);
        global.ER_obj.push_(obj);
        global.ER_name.push_(name);
    }
}

// Helper functions
function ER_readPropIdx(arr, idx) {
    return ER_readProp(arr, idx, arr[idx]);
}

function ER_writePropIdx(obj, idx, value) {
    return obj[idx] = ER_writeProp(obj, idx, value);
}

function ER_writePropIdxFunc(obj, idx, value, id) {
    return obj[idx] = ER_writePropFunc(obj, idx, value, id);
}

function ER_writePropIdxStrict(obj, idx, value) {
    "use strict";
    return obj[idx] = ER_writeProp(obj, idx, value);
}

function ER_writePropIdxFuncStrict(obj, idx, value, id) {
    "use strict";
    return obj[idx] = ER_writePropFunc(obj, idx, value, id);
}

function ER_preIncProp(obj, idx) {
    var v = ++obj[idx];
    ER_writeProp(obj, idx, v);
    return v;
}

function ER_preIncPropStrict(obj, idx) {
    "use strict";
    var v = ++obj[idx];
    ER_writeProp(obj, idx, v);
    return v;
}

function ER_preDecProp(obj, idx) {
    var v = --obj[idx];
    ER_writeProp(obj, idx, v);
    return v;
}

function ER_preDecPropStrict(obj, idx) {
    "use strict";
    var v = --obj[idx];
    ER_writeProp(obj, idx, v);
    return v;
}

function ER_postIncProp(obj, idx) {
    var v = obj[idx]++;
    ER_writeProp(obj, idx, obj[idx]);
    return v;
}

function ER_postIncPropStrict(obj, idx) {
    "use strict";
    var v = obj[idx]++;
    ER_writeProp(obj, idx, obj[idx]);
    return v;
}

function ER_postDecProp(obj, idx) {
    var v = obj[idx]--;
    ER_writeProp(obj, idx, obj[idx]);
    return v;
}

function ER_postDecPropStrict(obj, idx) {
    "use strict";
    var v = obj[idx]--;
    ER_writeProp(obj, idx, obj[idx]);
    return v;
}

function ER_deletePropIdx(obj, idx) {
    ER_deleteProp(obj, idx);
    return delete obj[idx];
}

function ER_deletePropIdxStrict(obj, idx) {
    "use strict";
    ER_deleteProp(obj, idx);
    return delete obj[idx];
}
