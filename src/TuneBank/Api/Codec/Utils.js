"use strict";

export {encodeURIComponent};

export function decodeURIComponentImpl(s) {
  try {
    return decodeURIComponent(s);
  } catch(error) {
    return null;
  }
}
