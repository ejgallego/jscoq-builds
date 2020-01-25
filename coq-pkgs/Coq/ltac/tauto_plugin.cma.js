function(aZ){"use strict";var
T="_vendor+v8.11+32bit/coq/plugins/ltac/tauto.ml",D="f",x="X2",f="X1",l="tauto_flags",w="id",k=aZ.jsoo_runtime,a=k.caml_new_string,R=k.caml_register_global;function
d(a,b){return a.length==1?a(b):k.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):k.caml_call_gen(a,[b,c])}function
p(a,b,c,d){return a.length==3?a(b,c,d):k.caml_call_gen(a,[b,c,d])}function
S(a,b,c,d,e){return a.length==4?a(b,c,d,e):k.caml_call_gen(a,[b,c,d,e])}function
u(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):k.caml_call_gen(a,[b,c,d,e,f])}var
e=k.caml_get_global_data(),O=a("core.nnpp.type"),y=a("tauto_plugin"),i=e.Names,Q=e.Ltac_plugin__Tacenv,q=e.Util,s=e.CAst,E=e.Mltop,z=e.Ltac_plugin__Tacinterp,C=e.Coqlib,j=e.Tacticals,c=e.Proofview,I=e.Pp,H=e.Assert_failure,o=e.EConstr,m=e.Tactics,n=e.Hipattern,F=e.Geninterp,az=e.Locusops,ak=e.Global,V=e.Stdlib,ab=e.Goptions;R(45,[0,0],"Tauto_plugin");d(E[9],y);var
aF=a(D),aG=a("x"),aB=a("core.not.type"),aA=[22,0],ax=[0,a(T),196,49],au=a(f),av=a(x),aw=a(w),as=a(f),ao=a(f),ap=a(x),aq=a(w),am=a(f),aj=a(f),ah=a(f),X=a(l),Y=[0,a(T),61,12],U=a("tauto: anomaly"),W=a(l),_=[0,a("Intuition"),[0,a("Negation"),[0,a("Unfolding"),0]]],$=a("unfolding of not in intuition"),ae=[0,0,0],aD=[0,1,0,1,1,0],aE=[0,0,0,0,0,0],aH=[0,a(l),[0,a(f),0]],aI=a("is_empty"),aJ=[0,a(l),[0,a(f),0]],aK=a("is_unit_or_eq"),aL=[0,a(l),[0,a(f),0]],aM=a("is_disj"),aN=[0,a(l),[0,a(f),0]],aO=a("is_conj"),aP=[0,a(l),[0,a(f),[0,a(x),[0,a(w),0]]]],aQ=a("flatten_contravariant_disj"),aR=[0,a(l),[0,a(f),[0,a(x),[0,a(w),0]]]],aS=a("flatten_contravariant_conj"),aT=a("apply_nnpp"),aU=a("reduction_not_iff"),aV=[0,a(D),0],aW=a("with_uniform_flags"),aX=[0,a(D),0],aY=a("with_power_flags");function
g(e,c){var
f=c[1],g=d(i[1][6],e),h=b(i[1][11][23],g,f),a=d(z[2][2],h);return a?a[1]:d(V[3],U)}var
G=d(F[1][1],W);function
t(c){var
e=c[1],f=d(i[1][6],X),a=b(i[1][11][23],f,e),g=a[2];if(b(F[1][2],a[1],G))return g;throw[0,H,Y]}var
A=[0,1];function
Z(a){A[1]=a;return 0}var
aa=[0,0,$,_,function(a){return d(q[3],A)},Z];b(ab[4],0,aa);var
v=d(c[16],0),ac=d(I[7],0),ad=b(j[57][4],0,ac),r=d(c[39],ad),J=m[16];function
K(a,b){var
e=a?[0,[0,a[1]]]:0,f=S(m[143],1,e,0,b);return d(c[39],f)}function
B(a){return d(m[87],a)}function
L(a){return d(m[76],[0,a,0])}var
M=m[42],af=b(m[117],0,ae);function
ag(e,a){function
d(d){function
e(b){var
c=g(ah,a);return p(n[12],d,b,c)?v:r}return b(c[72][1],c[54],e)}return b(c[72][1],c[55],d)}function
ai(e,a){function
d(d){function
e(b){var
c=t(a)[5]?n[15]:n[14];return p(c,d,b,g(aj,a))?v:r}return b(c[72][1],c[54],e)}return b(c[72][1],c[55],d)}function
N(a,c){var
e=b(o[59],a,c);if(e){var
g=b(o[92],a,c)[1],f=b(o[3],a,g);return 11===f[0]?2===d(ak[40],f[1][1])[1][6]?1:0:0}return e}function
al(e,d){function
a(e){function
a(b){var
a=t(d),c=g(am,d),f=a[2]?N(b,c)?0:1:0;if(!f)if(u(n[6],[0,a[4]],[0,a[1]],e,b,c))return v;return r}return b(c[72][1],c[54],a)}return b(c[72][1],c[55],a)}function
an(f,a){function
e(k){function
e(c){var
e=t(a),l=g(ao,a),m=g(ap,a),f=g(aq,a),h=u(n[5],[0,e[3]],[0,e[1]],k,c,l);if(h){var
i=h[1][2],s=function(b,a){return p(o[35],b,0,a)},v=p(q[22][16],s,i,m),w=function(a){return J},x=b(j[57][23],w,i),y=[0,x,[0,B(f),[0,af,[0,M,0]]]],z=d(j[57][22],y),A=[0,L(b(o[76],c,f)),0],C=[0,K([0,z],v),A];return d(j[57][22],C)}return r}return b(c[72][1],c[54],e)}return b(c[72][1],c[55],e)}function
ar(e,d){function
a(e){function
a(b){var
a=t(d),c=g(as,d),f=a[2]?N(b,c)?0:1:0;if(!f)if(u(n[4],[0,a[4]],[0,a[1]],e,b,c))return v;return r}return b(c[72][1],c[54],a)}return b(c[72][1],c[55],a)}function
at(f,a){function
e(i){function
e(c){var
e=t(a),k=g(au,a),l=g(av,a),f=g(aw,a),h=u(n[3],[0,e[3]],[0,e[1]],i,c,k);if(h){var
s=h[1][2],v=function(b,a){var
c=p(o[35],a,0,l),e=[0,S(m[109],0,0,b+1|0,0),[0,M,0]],g=[0,J,[0,B(f),e]];return K([0,d(j[57][22],g)],c)},w=b(q[22][13],v,s),x=L(b(o[76],c,f)),y=d(j[57][22],w);return b(j[57][3],y,x)}return r}return b(c[72][1],c[54],e)}return b(c[72][1],c[55],e)}function
ay(h,f){if(0===d(q[3],A))var
e=aA;else{var
a=d(C[2],aB),g=0;switch(a[0]){case
0:var
c=[0,a[1]];break;case
1:var
c=[1,a[1]];break;default:throw[0,H,ax]}var
e=[0,b(s[1],0,[10,[5,[0,[0,0,[0,[0,c,0]]],g]],az[6]])]}return b(z[23],f,e)}function
aC(g,f){function
a(g){if(d(C[3],O)){var
a=d(C[2],O),e=d(j[57][61],a);return b(c[72][1],e,B)}var
f=d(I[7],0);return b(j[57][4],0,f)}var
e=d(c[16],0);return b(c[17],e,a)}function
P(e,o,a){var
f=d(i[1][6],aF),g=b(s[1],0,f),h=d(i[1][6],aG),c=b(s[1],0,h),j=a[3],k=a[2],l=[0,p(i[1][11][4],c[1],[0,G,e],a[1]),k,j],m=[3,b(s[1],0,[0,[1,g],[0,[2,[1,c]],0]])],n=[29,b(s[1],0,m)];return b(z[23],l,n)}function
h(f,a,e){function
g(a){return d(i[1][6],a)}var
h=b(q[22][68],g,e);function
j(a){return[0,a]}var
c=[0,y,a],k=b(q[22][68],j,h);p(Q[16],0,c,[0,f]);var
l=[28,[0,k,[31,b(s[1],0,[0,[0,c,0],0])]]];function
m(c){var
b=d(i[1][6],a);return u(Q[10],1,1,0,b,l)}return b(E[13],m,y)}h(ag,aI,aH);h(ai,aK,aJ);h(ar,aM,aL);h(al,aO,aN);h(at,aQ,aP);h(an,aS,aR);h(aC,aT,0);h(ay,aU,0);h(function(a,b){return P(aD,a,b)},aW,aV);h(function(a,b){return P(aE,a,b)},aY,aX);R(65,[0],"Tauto_plugin__Tauto");return}
