function(ad){"use strict";var
P=250,O="Ascii",s=246,N="ascii",d=ad.jsoo_runtime,L=d.caml_ml_string_length,c=d.caml_new_string,K=d.caml_obj_tag,M=d.caml_register_global,n=d.caml_string_get,ac=d.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):d.caml_call_gen(a,[b])}function
e(a,b,c){return a.length==2?a(b,c):d.caml_call_gen(a,[b,c])}function
r(a,b,c,e){return a.length==3?a(b,c,e):d.caml_call_gen(a,[b,c,e])}var
a=d.caml_get_global_data(),o=c("ascii_syntax_plugin"),j=[0,c("Coq"),[0,c("Strings"),[0,c(O),0]]],q=c("char_scope"),t=a.Mltop,h=a.Util,k=a.Coqlib,i=a.DAst,C=a.CamlinternalLazy,g=a.Names,I=a.Notation,ab=a.Option,aa=a.Stdlib,$=a.Stdlib__printf,X=a.Pp,Z=a.CErrors,R=a.Libnames,Q=a.Globnames;b(t[10],o);var
l=[248,c("Ascii_syntax.Non_closed_ascii"),d.caml_fresh_oo_id(0)],_=[0,[4,0,[0,2,3],0,0],c("%03d")],W=c("Expects a single character or a three-digits ascii code."),Y=[0,c("interp_ascii_string")],V=c(O),U=c("Ascii interpretation"),S=c(N),T=c(N);function
p(a){var
c=e(h[17][14],g[1][6],a);return b(g[5][4],c)}function
u(c,a){var
d=b(g[1][6],a),f=p(c);return e(Q[24],f,d)}function
v(c,a){var
d=b(g[1][6],a),f=p(c);return e(R[15],f,d)}function
m(d,c){var
a=b(i[1],d);return 0===a[0]?e(g[68][1],a[1],c):0}var
w=v(j,S),x=u(j,T),y=[0,[0,x,0],1],z=[3,y];function
A(a){return r(k[1],U,j,a)}var
f=[s,function(a){return A(V)}];function
B(a,g){function
c(d,b){if(0===d)return 0;var
f=c(d-1|0,b/2|0),g=0,h=0===(b%2|0)?k[27]:k[26];return[0,e(i[3],a,[0,h,g]),f]}var
d=K(f),h=c(8,g),j=0,l=P===d?f[1]:s===d?b(C[2],f):f,m=[4,e(i[3],a,[0,l,j]),h];return e(i[3],a,m)}function
D(f,a){if(1===L(a))var
e=n(a,0);else{if(3===L(a)){var
g=n(a,0);if(b(h[11],g)){var
i=n(a,1);if(b(h[11],i)){var
j=n(a,2);if(b(h[11],j))var
e=d.caml_int_of_string(a),c=1;else
var
c=0}else
var
c=0}else
var
c=0}else
var
c=0;if(!c)var
k=b(X[3],W),e=r(Z[6],f,Y,k)}return B(f,e)}function
E(c){function
a(c,b){if(b){var
d=b[2],e=b[1];if(m(e,k[26]))return 1+(2*a(c-1|0,d)|0)|0;if(m(e,k[27]))return 2*a(c-1|0,d)|0}else
if(0===c)return 0;throw l}try{var
d=[0,function(e){var
c=b(i[1],e);if(4===c[0]){var
d=K(f),g=c[2],h=c[1],j=P===d?f[1]:s===d?b(C[2],f):f;if(m(h,j))return a(8,g)}throw l}(c)];return d}catch(a){a=ac(a);if(a===l)return 0;throw a}}function
F(a){if(32<=a)if(!(126<a)){var
c=b(aa[18],a);return e(h[15][1],1,c)}return e($[4],_,a)}function
G(a){var
b=E(a[1]);return e(ab[16],F,b)}function
H(c,a){function
d(d){return b(c,a)}return e(t[14],d,o)}r(I[20],0,q,[0,D,G]);H(I[22],[0,0,q,[0,q],[0,w,j],[0,z,0],1]);var
J=[0,o,l,p,u,v,m,j,w,x,y,z,A,f,B,D,E,F,G,H];M(25,J,"Ascii_syntax_plugin.Ascii_syntax");M(26,[0,J],"Ascii_syntax_plugin");return}
