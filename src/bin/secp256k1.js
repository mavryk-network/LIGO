function _typeof(obj) {
  "@babel/helpers - typeof";
  return (
    (_typeof =
      "function" == typeof Symbol && "symbol" == typeof Symbol.iterator
        ? function (obj) {
            return typeof obj;
          }
        : function (obj) {
            return obj &&
              "function" == typeof Symbol &&
              obj.constructor === Symbol &&
              obj !== Symbol.prototype
              ? "symbol"
              : typeof obj;
          }),
    _typeof(obj)
  );
}
var _SECP256K1 = (function () {
  var _scriptDir =
    typeof document !== "undefined" && document.currentScript
      ? document.currentScript.src
      : undefined;
  if (typeof __filename !== "undefined") _scriptDir = _scriptDir || __filename;
  return function (_SECP256K1) {
    _SECP256K1 = _SECP256K1 || {};
    var a;
    a || (a = typeof _SECP256K1 !== "undefined" ? _SECP256K1 : {});
    var k, l;
    a.ready = new Promise(function (b, c) {
      k = b;
      l = c;
    });
    var p = Object.assign({}, a),
      r =
        "object" ==
        (typeof window === "undefined" ? "undefined" : _typeof(window)),
      u = "function" == typeof importScripts,
      v =
        "object" ==
          (typeof process === "undefined" ? "undefined" : _typeof(process)) &&
        "object" == _typeof(process.versions) &&
        "string" == typeof process.versions.node,
      w = "",
      y,
      z,
      A;
    if (v) {
      var fs = require("fs"),
        B = require("path");
      w = u ? B.dirname(w) + "/" : __dirname + "/";
      y = function y(b, c) {
        b = b.startsWith("file://") ? new URL(b) : B.normalize(b);
        return fs.readFileSync(b, c ? void 0 : "utf8");
      };
      A = function A(b) {
        b = y(b, !0);
        b.buffer || (b = new Uint8Array(b));
        return b;
      };
      z = function z(b, c, f) {
        b = b.startsWith("file://") ? new URL(b) : B.normalize(b);
        fs.readFile(b, function (d, e) {
          d ? f(d) : c(e.buffer);
        });
      };
      1 < process.argv.length && process.argv[1].replace(/\\/g, "/");
      process.argv.slice(2);
      process.on("uncaughtException", function (b) {
        throw b;
      });
      process.on("unhandledRejection", function (b) {
        throw b;
      });
      a.inspect = function () {
        return "[Emscripten Module object]";
      };
    } else if (r || u)
      u
        ? (w = self.location.href)
        : "undefined" != typeof document &&
          document.currentScript &&
          (w = document.currentScript.src),
        _scriptDir && (w = _scriptDir),
        0 !== w.indexOf("blob:")
          ? (w = w.substr(0, w.replace(/[?#].*/, "").lastIndexOf("/") + 1))
          : (w = ""),
        (y = function y(b) {
          var c = new XMLHttpRequest();
          c.open("GET", b, !1);
          c.send(null);
          return c.responseText;
        }),
        u &&
          (A = function A(b) {
            var c = new XMLHttpRequest();
            c.open("GET", b, !1);
            c.responseType = "arraybuffer";
            c.send(null);
            return new Uint8Array(c.response);
          }),
        (z = function z(b, c, f) {
          var d = new XMLHttpRequest();
          d.open("GET", b, !0);
          d.responseType = "arraybuffer";
          d.onload = function () {
            200 == d.status || (0 == d.status && d.response)
              ? c(d.response)
              : f();
          };
          d.onerror = f;
          d.send(null);
        });
    var aa = a.print || console.log.bind(console),
      C = a.printErr || console.warn.bind(console);
    Object.assign(a, p);
    p = null;
    var D;
    a.wasmBinary && (D = a.wasmBinary);
    var noExitRuntime = a.noExitRuntime || !0;
    "object" !=
      (typeof WebAssembly === "undefined"
        ? "undefined"
        : _typeof(WebAssembly)) && E("no native wasm support detected");
    var F,
      G = !1,
      H = "undefined" != typeof TextDecoder ? new TextDecoder("utf8") : void 0,
      I,
      J,
      N;
    function O() {
      var b = F.buffer;
      I = b;
      a.HEAP8 = new Int8Array(b);
      a.HEAP16 = new Int16Array(b);
      a.HEAP32 = new Int32Array(b);
      a.HEAPU8 = J = new Uint8Array(b);
      a.HEAPU16 = new Uint16Array(b);
      a.HEAPU32 = N = new Uint32Array(b);
      a.HEAPF32 = new Float32Array(b);
      a.HEAPF64 = new Float64Array(b);
    }
    var P = [],
      Q = [],
      R = [];
    function ea() {
      var b = a.preRun.shift();
      P.unshift(b);
    }
    var S = 0,
      T = null,
      U = null;
    function E(b) {
      if (a.onAbort) a.onAbort(b);
      b = "Aborted(" + b + ")";
      C(b);
      G = !0;
      b = new WebAssembly.RuntimeError(
        b + ". Build with -sASSERTIONS for more info."
      );
      l(b);
      throw b;
    }
    function V() {
      return W.startsWith("data:application/octet-stream;base64,");
    }
    var W;
    W = "secp256k1.wasm";
    if (!V()) {
      var X = W;
      W = a.locateFile ? a.locateFile(X, w) : w + X;
    }
    function fa() {
      var b = W;
      try {
        if (b == W && D) return new Uint8Array(D);
        if (A) return A(b);
        throw "both async and sync fetching of the wasm failed";
      } catch (c) {
        E(c);
      }
    }
    function ha() {
      if (!D && (r || u)) {
        if ("function" == typeof fetch && !W.startsWith("file://"))
          return fetch(W, {
            credentials: "same-origin",
          })
            .then(function (b) {
              if (!b.ok) throw "failed to load wasm binary file at '" + W + "'";
              return b.arrayBuffer();
            })
            ["catch"](function () {
              return fa();
            });
        if (z)
          return new Promise(function (b, c) {
            z(
              W,
              function (f) {
                b(new Uint8Array(f));
              },
              c
            );
          });
      }
      return Promise.resolve().then(function () {
        return fa();
      });
    }
    function Y(b) {
      for (; 0 < b.length; ) b.shift()(a);
    }
    var ia = [null, [], []],
      ja = {
        a: function a() {
          E("");
        },
        f: function f(b, c, _f) {
          J.copyWithin(b, c, c + _f);
        },
        d: function d(b) {
          var c = J.length;
          b >>>= 0;
          if (2147483648 < b) return !1;
          for (var f = 1; 4 >= f; f *= 2) {
            var d = c * (1 + 0.2 / f);
            d = Math.min(d, b + 100663296);
            var e = Math;
            d = Math.max(b, d);
            e = e.min.call(e, 2147483648, d + ((65536 - (d % 65536)) % 65536));
            a: {
              try {
                F.grow((e - I.byteLength + 65535) >>> 16);
                O();
                var g = 1;
                break a;
              } catch (ba) {}
              g = void 0;
            }
            if (g) return !0;
          }
          return !1;
        },
        e: function e() {
          return 52;
        },
        c: function c() {
          return 70;
        },
        b: function b(_b, c, f, d) {
          for (var e = 0, g = 0; g < f; g++) {
            var ba = N[c >> 2],
              ca = N[(c + 4) >> 2];
            c += 8;
            for (var K = 0; K < ca; K++) {
              var x = J[ba + K],
                L = ia[_b];
              if (0 === x || 10 === x) {
                x = 1 === _b ? aa : C;
                var m = L;
                for (var n = 0, q = n + NaN, t = n; m[t] && !(t >= q); ) ++t;
                if (16 < t - n && m.buffer && H) m = H.decode(m.subarray(n, t));
                else {
                  for (q = ""; n < t; ) {
                    var h = m[n++];
                    if (h & 128) {
                      var M = m[n++] & 63;
                      if (192 == (h & 224))
                        q += String.fromCharCode(((h & 31) << 6) | M);
                      else {
                        var da = m[n++] & 63;
                        h =
                          224 == (h & 240)
                            ? ((h & 15) << 12) | (M << 6) | da
                            : ((h & 7) << 18) |
                              (M << 12) |
                              (da << 6) |
                              (m[n++] & 63);
                        65536 > h
                          ? (q += String.fromCharCode(h))
                          : ((h -= 65536),
                            (q += String.fromCharCode(
                              55296 | (h >> 10),
                              56320 | (h & 1023)
                            )));
                      }
                    } else q += String.fromCharCode(h);
                  }
                  m = q;
                }
                x(m);
                L.length = 0;
              } else L.push(x);
            }
            e += ca;
          }
          N[d >> 2] = e;
          return 0;
        },
      };
    (function () {
      function b(e) {
        a.asm = e.exports;
        F = a.asm.g;
        O();
        Q.unshift(a.asm.h);
        S--;
        a.monitorRunDependencies && a.monitorRunDependencies(S);
        0 == S &&
          (null !== T && (clearInterval(T), (T = null)),
          U && ((e = U), (U = null), e()));
      }
      function c(e) {
        b(e.instance);
      }
      function f(e) {
        return ha()
          .then(function (g) {
            return WebAssembly.instantiate(g, d);
          })
          .then(function (g) {
            return g;
          })
          .then(e, function (g) {
            C("failed to asynchronously prepare wasm: " + g);
            E(g);
          });
      }
      var d = {
        a: ja,
      };
      S++;
      a.monitorRunDependencies && a.monitorRunDependencies(S);
      if (a.instantiateWasm)
        try {
          return a.instantiateWasm(d, b);
        } catch (e) {
          C("Module.instantiateWasm callback failed with error: " + e), l(e);
        }
      (function () {
        return D ||
          "function" != typeof WebAssembly.instantiateStreaming ||
          V() ||
          W.startsWith("file://") ||
          v ||
          "function" != typeof fetch
          ? f(c)
          : fetch(W, {
              credentials: "same-origin",
            }).then(function (e) {
              return WebAssembly.instantiateStreaming(e, d).then(
                c,
                function (g) {
                  C("wasm streaming compile failed: " + g);
                  C("falling back to ArrayBuffer instantiation");
                  return f(c);
                }
              );
            });
      })()["catch"](l);
      return {};
    })();
    a.___wasm_call_ctors = function () {
      return (a.___wasm_call_ctors = a.asm.h).apply(null, arguments);
    };
    a._secp256k1_fe_normalize = function () {
      return (a._secp256k1_fe_normalize = a.asm.i).apply(null, arguments);
    };
    a._secp256k1_fe_normalize_weak = function () {
      return (a._secp256k1_fe_normalize_weak = a.asm.j).apply(null, arguments);
    };
    a._secp256k1_fe_normalize_var = function () {
      return (a._secp256k1_fe_normalize_var = a.asm.k).apply(null, arguments);
    };
    a._secp256k1_fe_normalizes_to_zero = function () {
      return (a._secp256k1_fe_normalizes_to_zero = a.asm.l).apply(
        null,
        arguments
      );
    };
    a._secp256k1_fe_normalizes_to_zero_var = function () {
      return (a._secp256k1_fe_normalizes_to_zero_var = a.asm.m).apply(
        null,
        arguments
      );
    };
    a._secp256k1_fe_set_int = function () {
      return (a._secp256k1_fe_set_int = a.asm.n).apply(null, arguments);
    };
    a._secp256k1_fe_is_zero = function () {
      return (a._secp256k1_fe_is_zero = a.asm.o).apply(null, arguments);
    };
    a._secp256k1_fe_is_odd = function () {
      return (a._secp256k1_fe_is_odd = a.asm.p).apply(null, arguments);
    };
    a._secp256k1_fe_clear = function () {
      return (a._secp256k1_fe_clear = a.asm.q).apply(null, arguments);
    };
    a._secp256k1_fe_cmp_var = function () {
      return (a._secp256k1_fe_cmp_var = a.asm.r).apply(null, arguments);
    };
    a._secp256k1_fe_set_b32 = function () {
      return (a._secp256k1_fe_set_b32 = a.asm.s).apply(null, arguments);
    };
    a._secp256k1_fe_get_b32 = function () {
      return (a._secp256k1_fe_get_b32 = a.asm.t).apply(null, arguments);
    };
    a._secp256k1_fe_negate = function () {
      return (a._secp256k1_fe_negate = a.asm.u).apply(null, arguments);
    };
    a._secp256k1_fe_mul_int = function () {
      return (a._secp256k1_fe_mul_int = a.asm.v).apply(null, arguments);
    };
    a._secp256k1_fe_add = function () {
      return (a._secp256k1_fe_add = a.asm.w).apply(null, arguments);
    };
    a._secp256k1_fe_mul = function () {
      return (a._secp256k1_fe_mul = a.asm.x).apply(null, arguments);
    };
    a._secp256k1_fe_cmov = function () {
      return (a._secp256k1_fe_cmov = a.asm.y).apply(null, arguments);
    };
    a._secp256k1_fe_storage_cmov = function () {
      return (a._secp256k1_fe_storage_cmov = a.asm.z).apply(null, arguments);
    };
    a._secp256k1_fe_to_storage = function () {
      return (a._secp256k1_fe_to_storage = a.asm.A).apply(null, arguments);
    };
    a._secp256k1_fe_from_storage = function () {
      return (a._secp256k1_fe_from_storage = a.asm.B).apply(null, arguments);
    };
    a._secp256k1_fe_inv = function () {
      return (a._secp256k1_fe_inv = a.asm.C).apply(null, arguments);
    };
    a._secp256k1_fe_inv_var = function () {
      return (a._secp256k1_fe_inv_var = a.asm.D).apply(null, arguments);
    };
    a._secp256k1_fe_equal = function () {
      return (a._secp256k1_fe_equal = a.asm.E).apply(null, arguments);
    };
    a._secp256k1_fe_equal_var = function () {
      return (a._secp256k1_fe_equal_var = a.asm.F).apply(null, arguments);
    };
    a._secp256k1_fe_sqrt = function () {
      return (a._secp256k1_fe_sqrt = a.asm.G).apply(null, arguments);
    };
    a._secp256k1_scalar_clear = function () {
      return (a._secp256k1_scalar_clear = a.asm.H).apply(null, arguments);
    };
    a._secp256k1_scalar_set_int = function () {
      return (a._secp256k1_scalar_set_int = a.asm.I).apply(null, arguments);
    };
    a._secp256k1_scalar_get_bits = function () {
      return (a._secp256k1_scalar_get_bits = a.asm.J).apply(null, arguments);
    };
    a._secp256k1_scalar_get_bits_var = function () {
      return (a._secp256k1_scalar_get_bits_var = a.asm.K).apply(
        null,
        arguments
      );
    };
    a._secp256k1_scalar_add = function () {
      return (a._secp256k1_scalar_add = a.asm.L).apply(null, arguments);
    };
    a._secp256k1_scalar_cadd_bit = function () {
      return (a._secp256k1_scalar_cadd_bit = a.asm.M).apply(null, arguments);
    };
    a._secp256k1_scalar_set_b32 = function () {
      return (a._secp256k1_scalar_set_b32 = a.asm.N).apply(null, arguments);
    };
    a._secp256k1_scalar_get_b32 = function () {
      return (a._secp256k1_scalar_get_b32 = a.asm.O).apply(null, arguments);
    };
    a._secp256k1_scalar_is_zero = function () {
      return (a._secp256k1_scalar_is_zero = a.asm.P).apply(null, arguments);
    };
    a._secp256k1_scalar_negate = function () {
      return (a._secp256k1_scalar_negate = a.asm.Q).apply(null, arguments);
    };
    a._secp256k1_scalar_is_one = function () {
      return (a._secp256k1_scalar_is_one = a.asm.R).apply(null, arguments);
    };
    a._secp256k1_scalar_is_high = function () {
      return (a._secp256k1_scalar_is_high = a.asm.S).apply(null, arguments);
    };
    a._secp256k1_scalar_cond_negate = function () {
      return (a._secp256k1_scalar_cond_negate = a.asm.T).apply(null, arguments);
    };
    a._secp256k1_scalar_mul = function () {
      return (a._secp256k1_scalar_mul = a.asm.U).apply(null, arguments);
    };
    a._secp256k1_scalar_shr_int = function () {
      return (a._secp256k1_scalar_shr_int = a.asm.V).apply(null, arguments);
    };
    a._secp256k1_scalar_eq = function () {
      return (a._secp256k1_scalar_eq = a.asm.W).apply(null, arguments);
    };
    a._secp256k1_scalar_mul_shift_var = function () {
      return (a._secp256k1_scalar_mul_shift_var = a.asm.X).apply(
        null,
        arguments
      );
    };
    a._secp256k1_scalar_inverse = function () {
      return (a._secp256k1_scalar_inverse = a.asm.Y).apply(null, arguments);
    };
    a._secp256k1_scalar_inverse_var = function () {
      return (a._secp256k1_scalar_inverse_var = a.asm.Z).apply(null, arguments);
    };
    a._secp256k1_scalar_is_even = function () {
      return (a._secp256k1_scalar_is_even = a.asm._).apply(null, arguments);
    };
    a._secp256k1_ge_set_xy = function () {
      return (a._secp256k1_ge_set_xy = a.asm.$).apply(null, arguments);
    };
    a._secp256k1_ge_is_infinity = function () {
      return (a._secp256k1_ge_is_infinity = a.asm.aa).apply(null, arguments);
    };
    a._secp256k1_ge_neg = function () {
      return (a._secp256k1_ge_neg = a.asm.ba).apply(null, arguments);
    };
    a._secp256k1_ge_set_gej = function () {
      return (a._secp256k1_ge_set_gej = a.asm.ca).apply(null, arguments);
    };
    a._secp256k1_gej_set_infinity = function () {
      return (a._secp256k1_gej_set_infinity = a.asm.da).apply(null, arguments);
    };
    a._secp256k1_gej_clear = function () {
      return (a._secp256k1_gej_clear = a.asm.ea).apply(null, arguments);
    };
    a._secp256k1_ge_clear = function () {
      return (a._secp256k1_ge_clear = a.asm.fa).apply(null, arguments);
    };
    a._secp256k1_gej_set_ge = function () {
      return (a._secp256k1_gej_set_ge = a.asm.ga).apply(null, arguments);
    };
    a._secp256k1_gej_eq_x_var = function () {
      return (a._secp256k1_gej_eq_x_var = a.asm.ha).apply(null, arguments);
    };
    a._secp256k1_gej_neg = function () {
      return (a._secp256k1_gej_neg = a.asm.ia).apply(null, arguments);
    };
    a._secp256k1_gej_is_infinity = function () {
      return (a._secp256k1_gej_is_infinity = a.asm.ja).apply(null, arguments);
    };
    a._secp256k1_ge_is_valid_var = function () {
      return (a._secp256k1_ge_is_valid_var = a.asm.ka).apply(null, arguments);
    };
    a._secp256k1_gej_double_var = function () {
      return (a._secp256k1_gej_double_var = a.asm.la).apply(null, arguments);
    };
    a._secp256k1_gej_add_var = function () {
      return (a._secp256k1_gej_add_var = a.asm.ma).apply(null, arguments);
    };
    a._secp256k1_gej_add_ge_var = function () {
      return (a._secp256k1_gej_add_ge_var = a.asm.na).apply(null, arguments);
    };
    a._secp256k1_gej_add_ge = function () {
      return (a._secp256k1_gej_add_ge = a.asm.oa).apply(null, arguments);
    };
    a._secp256k1_gej_rescale = function () {
      return (a._secp256k1_gej_rescale = a.asm.pa).apply(null, arguments);
    };
    a._secp256k1_ge_to_storage = function () {
      return (a._secp256k1_ge_to_storage = a.asm.qa).apply(null, arguments);
    };
    a._secp256k1_ge_from_storage = function () {
      return (a._secp256k1_ge_from_storage = a.asm.ra).apply(null, arguments);
    };
    a._secp256k1_ge_storage_cmov = function () {
      return (a._secp256k1_ge_storage_cmov = a.asm.sa).apply(null, arguments);
    };
    a._secp256k1_ecmult_const = function () {
      return (a._secp256k1_ecmult_const = a.asm.ta).apply(null, arguments);
    };
    a._secp256k1_eckey_pubkey_parse = function () {
      return (a._secp256k1_eckey_pubkey_parse = a.asm.ua).apply(
        null,
        arguments
      );
    };
    a._secp256k1_eckey_pubkey_serialize = function () {
      return (a._secp256k1_eckey_pubkey_serialize = a.asm.va).apply(
        null,
        arguments
      );
    };
    a._secp256k1_context_create = function () {
      return (a._secp256k1_context_create = a.asm.wa).apply(null, arguments);
    };
    a._malloc = function () {
      return (a._malloc = a.asm.xa).apply(null, arguments);
    };
    a._free = function () {
      return (a._free = a.asm.ya).apply(null, arguments);
    };
    a._secp256k1_context_clone = function () {
      return (a._secp256k1_context_clone = a.asm.za).apply(null, arguments);
    };
    a._secp256k1_ec_pubkey_parse = function () {
      return (a._secp256k1_ec_pubkey_parse = a.asm.Aa).apply(null, arguments);
    };
    a._secp256k1_ec_pubkey_serialize = function () {
      return (a._secp256k1_ec_pubkey_serialize = a.asm.Ba).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_signature_parse_der = function () {
      return (a._secp256k1_ecdsa_signature_parse_der = a.asm.Ca).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_signature_parse_compact = function () {
      return (a._secp256k1_ecdsa_signature_parse_compact = a.asm.Da).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_signature_serialize_der = function () {
      return (a._secp256k1_ecdsa_signature_serialize_der = a.asm.Ea).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_signature_serialize_compact = function () {
      return (a._secp256k1_ecdsa_signature_serialize_compact = a.asm.Fa).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_signature_normalize = function () {
      return (a._secp256k1_ecdsa_signature_normalize = a.asm.Ga).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_verify = function () {
      return (a._secp256k1_ecdsa_verify = a.asm.Ha).apply(null, arguments);
    };
    a._secp256k1_ecdsa_sign = function () {
      return (a._secp256k1_ecdsa_sign = a.asm.Ia).apply(null, arguments);
    };
    a._secp256k1_ec_seckey_verify = function () {
      return (a._secp256k1_ec_seckey_verify = a.asm.Ja).apply(null, arguments);
    };
    a._secp256k1_ec_pubkey_create = function () {
      return (a._secp256k1_ec_pubkey_create = a.asm.Ka).apply(null, arguments);
    };
    a._secp256k1_ec_seckey_negate = function () {
      return (a._secp256k1_ec_seckey_negate = a.asm.La).apply(null, arguments);
    };
    a._secp256k1_ec_pubkey_negate = function () {
      return (a._secp256k1_ec_pubkey_negate = a.asm.Ma).apply(null, arguments);
    };
    a._secp256k1_ec_seckey_tweak_add = function () {
      return (a._secp256k1_ec_seckey_tweak_add = a.asm.Na).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ec_pubkey_tweak_add = function () {
      return (a._secp256k1_ec_pubkey_tweak_add = a.asm.Oa).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ec_seckey_tweak_mul = function () {
      return (a._secp256k1_ec_seckey_tweak_mul = a.asm.Pa).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ec_pubkey_tweak_mul = function () {
      return (a._secp256k1_ec_pubkey_tweak_mul = a.asm.Qa).apply(
        null,
        arguments
      );
    };
    a._secp256k1_context_randomize = function () {
      return (a._secp256k1_context_randomize = a.asm.Ra).apply(null, arguments);
    };
    a._secp256k1_ec_pubkey_combine = function () {
      return (a._secp256k1_ec_pubkey_combine = a.asm.Sa).apply(null, arguments);
    };
    a._secp256k1_ecdsa_recoverable_signature_parse_compact = function () {
      return (a._secp256k1_ecdsa_recoverable_signature_parse_compact =
        a.asm.Ta).apply(null, arguments);
    };
    a._secp256k1_ecdsa_recoverable_signature_serialize_compact = function () {
      return (a._secp256k1_ecdsa_recoverable_signature_serialize_compact =
        a.asm.Ua).apply(null, arguments);
    };
    a._secp256k1_ecdsa_recoverable_signature_convert = function () {
      return (a._secp256k1_ecdsa_recoverable_signature_convert =
        a.asm.Va).apply(null, arguments);
    };
    a._secp256k1_ecdsa_sign_recoverable = function () {
      return (a._secp256k1_ecdsa_sign_recoverable = a.asm.Wa).apply(
        null,
        arguments
      );
    };
    a._secp256k1_ecdsa_recover = function () {
      return (a._secp256k1_ecdsa_recover = a.asm.Xa).apply(null, arguments);
    };
    a._secp256k1_scalar_const = function () {
      return (a._secp256k1_scalar_const = a.asm.Ya).apply(null, arguments);
    };
    a._secp256k1_fe_const = function () {
      return (a._secp256k1_fe_const = a.asm.Za).apply(null, arguments);
    };
    a._secp256k1_ge_of_fields = function () {
      return (a._secp256k1_ge_of_fields = a.asm._a).apply(null, arguments);
    };
    a._secp256k1_fe_storage_const = function () {
      return (a._secp256k1_fe_storage_const = a.asm.$a).apply(null, arguments);
    };
    a._secp256k1_gej_of_fields = function () {
      return (a._secp256k1_gej_of_fields = a.asm.ab).apply(null, arguments);
    };
    a._secp256k1_ge_storage_of_fields = function () {
      return (a._secp256k1_ge_storage_of_fields = a.asm.bb).apply(
        null,
        arguments
      );
    };
    var Z;
    U = function ka() {
      Z || la();
      Z || (U = ka);
    };
    function la() {
      function b() {
        if (!Z && ((Z = !0), (a.calledRun = !0), !G)) {
          Y(Q);
          k(a);
          if (a.onRuntimeInitialized) a.onRuntimeInitialized();
          if (a.postRun)
            for (
              "function" == typeof a.postRun && (a.postRun = [a.postRun]);
              a.postRun.length;

            ) {
              var c = a.postRun.shift();
              R.unshift(c);
            }
          Y(R);
        }
      }
      if (!(0 < S)) {
        if (a.preRun)
          for (
            "function" == typeof a.preRun && (a.preRun = [a.preRun]);
            a.preRun.length;

          )
            ea();
        Y(P);
        0 < S ||
          (a.setStatus
            ? (a.setStatus("Running..."),
              setTimeout(function () {
                setTimeout(function () {
                  a.setStatus("");
                }, 1);
                b();
              }, 1))
            : b());
      }
    }
    if (a.preInit)
      for (
        "function" == typeof a.preInit && (a.preInit = [a.preInit]);
        0 < a.preInit.length;

      )
        a.preInit.pop()();
    la();
    return _SECP256K1.ready;
  };
})();
if (
  (typeof exports === "undefined" ? "undefined" : _typeof(exports)) ===
    "object" &&
  (typeof module === "undefined" ? "undefined" : _typeof(module)) === "object"
)
  module.exports = _SECP256K1;
else if (typeof define === "function" && define["amd"])
  define([], function () {
    return _SECP256K1;
  });
else if (
  (typeof exports === "undefined" ? "undefined" : _typeof(exports)) === "object"
)
  exports["_SECP256K1"] = _SECP256K1;
