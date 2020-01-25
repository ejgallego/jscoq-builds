function(C){"use strict";var
c=C.jsoo_runtime,b=c.caml_new_string,r=c.caml_register_global;function
d(a,b){return a.length==1?a(b):c.caml_call_gen(a,[b])}function
e(a,b,d){return a.length==2?a(b,d):c.caml_call_gen(a,[b,d])}function
B(a,b,d,e){return a.length==3?a(b,d,e):c.caml_call_gen(a,[b,d,e])}var
a=c.caml_get_global_data(),g=b("float_syntax_plugin"),i=[0,b("Coq"),[0,b("Floats"),[0,b("PrimFloat"),0]]],f=b("float_scope"),j=a.Mltop,h=a.Names,q=a.Notation,v=a.NumTok,w=a.Stdlib,x=a.Float64,y=a.DAst,t=a.Libnames,s=a.Stdlib__list;r(6,[0,0],"Float_syntax_plugin");d(j[9],g);var
z=b("-"),u=b(""),A=b("float");function
k(a){var
b=e(s[19],h[1][6],a);return d(h[5][4],b)}function
l(b,a){var
c=d(h[1][6],a),f=k(b);return e(t[15],f,c)}function
m(b,a){var
c=a[2],f=0===a[1]?u:z,g=d(v[3],c),h=e(w[17],f,g),i=[16,d(x[5],h)];return e(y[3],b,i)}function
n(a){return 0}function
o(b,a){function
c(c){return d(b,a)}return e(j[13],c,g)}var
p=l(i,A);B(q[19],0,f,[0,m,n]);o(q[23],[0,0,f,[0,f],[0,p,i],0,0]);r(16,[0,g,k,l,m,n,o,i,p,f],"Float_syntax_plugin__Float_syntax");return}
