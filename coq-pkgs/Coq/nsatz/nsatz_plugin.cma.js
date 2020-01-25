function(ed){"use strict";var
_="*",bd="term computed",bc="+",bb="@[%s@]",l=246,aB="(",bl="_vendor+v8.11+32bit/coq/plugins/nsatz/polynom.ml",bf="+ ",ba="...",bk="h",aA=" ",$="1",a_="nsatz_compute",a$="print_pol dans dansideal",bj=")*",be="c",C="",bh=113,ai="^",G="0",bg="u",P="\n",bi=248,aa="_vendor+v8.11+32bit/coq/plugins/nsatz/nsatz.ml",o=250,p=ed.jsoo_runtime,f=p.caml_check_bound,az=p.caml_equal,a7=p.caml_fresh_oo_id,ay=p.caml_int_compare,a9=p.caml_int_of_string,eb=p.caml_list_of_js_array,w=p.caml_make_vect,d=p.caml_new_string,n=p.caml_obj_tag,O=p.caml_register_global,Z=p.caml_string_notequal,a6=p.caml_trampoline,ax=p.caml_trampoline_return,z=p.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):p.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):p.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):p.caml_call_gen(a,[b,c,d])}function
a8(a,b,c,d,e){return a.length==4?a(b,c,d,e):p.caml_call_gen(a,[b,c,d,e])}function
ec(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):p.caml_call_gen(a,[b,c,d,e,f])}var
i=p.caml_get_global_data(),aq=[0,0,0],B=[0,0],K=[0,1],Q=[0,2],aw=d("nsatz_plugin"),aj=i.Flags,ab=i.Pp,aE=i.Feedback,aD=i.Stdlib__format,e=i.Stdlib,c=i.Util,H=i.Assert_failure,ak=i.Failure,aF=i.Stdlib__char,D=i.Stdlib__hashtbl,h=i.Stdlib__list,ap=i.CList,I=i.Stdlib__printf,aK=i.Heap,ae=i.Not_found,ao=i.Stdlib__string,k=i.Num,m=i.CamlinternalLazy,u=i.Constr,a3=i.CErrors,aT=i.Coqlib,aU=i.UnivGen,a4=i.EConstr,a1=i.Unix,v=i.Big_int,ar=i.Ratio,cC=i.Stdlib__array,d2=i.Tactics,d4=i.Mltop,d7=i.Stdarg,d8=i.Genarg,ea=i.Ltac_plugin__Tacentries;O(154,[0,0,0,0,0,0],"Nsatz_plugin");var
bm=[0,[18,[1,[0,0,d(C)]],[2,0,[17,0,0]]],d(bb)],br=[0,d(bl),365,43],bQ=d(ba),bt=d(")"),bu=d(aB),bv=d(C),bw=d(C),bx=d(G),bO=d(ba),bP=d(bc),bz=d(C),bA=d(G),bB=d($),bD=d(bj),bE=d(aB),bF=d(_),bG=d(G),bH=d($),bI=d(ai),bJ=d(ai),bK=d(bj),bL=d(aB),bM=d(ai),bN=d(_),bC=d(bc),by=d(G),bV=d("div_pol1"),bW=[0,d(bl),477,16],bX=d(P),bY=d("x:"),bZ=d(P),b0=d("r:"),b1=d(P),b2=d("q:"),b3=d(P),b4=d("p:"),b5=d("div_pol:\n"),bT=d(P),bS=d("\n{ "),bU=d("}"),bR=[0,[18,[1,[0,0,d(C)]],[2,0,[17,0,0]]],d(bb)],bs=d(bg),bq=d("non"),bo=[0,0],bp=[0,1],cU=d(P),cT=d(C),cV=d("lp:\n"),cS=d("p: "),cR=d("homogeneous polynomials"),cW=d("computed"),cO=d("new polynomial: "),cN=d(be),cQ=d(bk),cP=d(G),cM=d("computation of the Groebner basis"),cL=d("coefficient: "),cK=d("verif sum: "),cJ=d("r: "),cG=d("remainder: "),cH=d("polynomial reduced to 0"),cI=d("r ok"),cE=[0,[12,91,[4,3,0,0,[12,44,[4,3,0,0,[12,93,0]]]]],d("[%i,%i]")],cD=[0,0,0],cA=d(" terms"),cB=d(" + "),cz=d(a$),cy=d(a$),cj=d(G),ck=d($),cl=d(ai),ce=d(_),cf=d(_),cg=d(_),ch=d($),ci=d(C),cm=d("-1"),cn=d(G),co=d($),cr=d(bf),cs=d(C),ct=d(aA),cu=d("-"),cp=d("- "),cq=d(bf),cv=d(G),cw=d(C),cx=d(aA),cd=d(aA),cb=d(" i="),cc=d("lv= "),b$=d("]"),ca=d("["),b9=[0,0],b_=[0,1],cF=d("Nsatz_plugin.Ideal.Make(P).NotInIdealUpdate"),b6=d("Nsatz_plugin.Ideal.NotInIdeal"),dx=[0,d(aa),243,9],d3=d("nsatz cannot solve this problem"),d1=d("core.eq.refl"),dZ=d(bd),dM=[0,[11,d("number of parameters: "),[4,3,0,0,0]],d("number of parameters: %i")],dJ=[0,[12,120,[4,3,0,0,0]],d("x%i")],dI=d("nsatz: bad parameter"),dQ=d("computation without sugar"),dR=d("computation with sugar"),dS=d("ordre lexico computation without sugar"),dT=d("ordre lexico computation with sugar"),dU=d("computation without sugar, division by pairs"),dV=d("computation with sugar, division by pairs"),dW=d("ordre lexico computation without sugar, division by pairs"),dX=d("ordre lexico computation with sugar, division by pairs"),dK=eb([d("a"),d("b"),d(be),d("d"),d("e"),d("f"),d("g"),d(bk),d("i"),d("j"),d("k"),d("l"),d("m"),d("n"),d("o"),d("p"),d("q"),d("r"),d("s"),d("t"),d(bg),d("v"),d("w"),d("x"),d("y"),d("z")]),dL=d("cert ok"),dN=d(bd),dO=[0,d(aa),484,18],dP=[0,d(aa),472,14],dH=[0,d(aa),501,8],dG=[0,d(aa),439,11],dF=[0,0,0],dE=[0,[11,d("time: "),[18,[1,[0,0,d(C)]],[8,0,[0,1,10],[0,3],[17,0,[12,115,0]]]]],d("time: @[%10.3f@]s")],dD=[0,[11,d("useful spolynomials: "),[4,3,0,0,[12,32,0]]],d("useful spolynomials: %i ")],dC=[0,[11,d("useless spolynomials: "),[4,3,0,0,0]],d("useless spolynomials: %i")],dz=[0,0,0],dA=[0,0,0],dB=d(G),dw=[0,0],c_=d("plugins.setoid_ring.pexpr"),c$=d("plugins.setoid_ring.const"),da=d("plugins.setoid_ring.var"),db=d("plugins.setoid_ring.add"),dc=d("plugins.setoid_ring.sub"),dd=d("plugins.setoid_ring.mul"),de=d("plugins.setoid_ring.opp"),df=d("plugins.setoid_ring.pow"),dg=d("core.list.type"),dh=d("core.list.nil"),di=d("core.list.cons"),dj=d("num.Z.type"),dk=d("num.Z.Z0"),dl=d("num.Z.Zpos"),dm=d("num.Z.Zneg"),dp=d("num.pos.xI"),dr=d("num.pos.xO"),ds=d("num.pos.xH"),dt=d("num.N.N0"),du=d("num.N.Npos"),d9=d(a_),d$=d(a_);function
aC(c){return aj[4][1]?(a(aD[bh],bm,c),b(e[52],e[28])):0}function
bn(a){return 0}function
s(d){var
c=aj[4][1];if(c){var
e=b(ab[3],d);return a(aE[9],0,e)}return c}function
x(d){var
c=aj[4][1];if(c){var
e=b(d,0),f=b(ab[3],e);return a(aE[9],0,f)}return c}O(160,[0,aC,bn,x,s],"Nsatz_plugin__Utile");function
aG(g){function
G(a){return b(g[14],[0,a])}var
d=G(0),T=G(1);function
I(a){return[0,b(g[14],a)]}var
k=I(bo),U=I(bp);function
V(a){return[1,a,[0,k,U]]}function
J(g,b){if(0===b)return[0,T];var
e=w(a(c[4],b,1),[0,d]);f(e,b)[1+b]=[0,T];return[1,g,e]}function
ag(a){return 0===a[0]?1:0}function
ah(a){return 0===a[0]?a[1]:b(e[3],bq)}function
ai(b){return 0===b[0]?a(g[1],b[1],d)?1:0:0}function
r(a){return 0===a[0]?0:a[1]}function
K(b){if(0===b[0])return 0;var
d=b[2],f=b[1];function
g(c,b){var
d=K(c);return a(e[6],d,b)}return j(c[24][18],g,d,f)}function
aj(b){var
d=0;function
f(c,b){var
d=K(c);return a(e[6],d,b)}return j(c[24][18],f,b,d)}function
m(d,b){if(0===d[0]){var
f=d[1];if(0===b[0])return a(g[1],f,b[1])}else{var
h=d[2],i=d[1];if(0!==b[0]){var
e=i===b[1]?1:0,k=b[2];return e?j(c[24][37],m,h,k):e}}return 0}function
W(h){if(0===h[0])return h;var
i=h[2],o=h[1],j=a(c[5],i.length-1,1),e=[0,j];for(;;){if(0<b(c[3],e)){var
k=b(c[3],e);if(m(f(i,k)[1+k],[0,d])){var
p=b(c[3],e);e[1]=a(c[5],p,1);continue}}if(0<=b(c[3],e)){if(0===b(c[3],e))return f(i,0)[1];if(b(c[3],e)===j)return h;var
q=b(c[3],e),l=w(a(c[4],q,1),[0,d]),n=b(c[3],e),r=0;if(!(n<0)){var
g=r;for(;;){var
s=f(i,g)[1+g];f(l,g)[1+g]=s;var
t=g+1|0;if(n!==g){var
g=t;continue}break}}return[1,o,l]}return[0,d]}}function
h(d,b){if(1===b[0]){var
e=b[2];if(b[1]===d)return a(c[5],e.length-1,1)}return 0}function
X(f){if(0===f[0])return 0;var
d=[0,0],g=f[2];function
h(g,f){var
h=X(f),i=a(c[4],g,h),j=b(c[3],d);d[1]=a(e[6],j,i);return 0}a(c[24][14],h,g);return b(c[3],d)}function
t(b){if(0===b[0])return[0,b[1]];var
d=b[1];return[1,d,a(c[24][15],t,b[2])]}function
v(e,b,a){if(1===a[0]){var
c=a[2];if(a[1]===e)return b<c.length-1?f(c,b)[1+b]:[0,d]}return 0===b?a:[0,d]}function
o(j,b){if(0===j[0]){var
D=j[1];if(0===b[0])var
q=[0,a(g[5],D,b[1])];else{var
s=b[2],E=b[1],u=a(c[24][15],t,s),F=o(j,f(s,0)[1]);f(u,0)[1]=F;var
q=[1,E,u]}var
r=q}else{var
l=j[2],i=j[1];if(0===b[0]){var
x=a(c[24][15],t,l),G=o(f(l,0)[1],b);f(x,0)[1]=G;var
y=[1,i,x]}else{var
z=b[2],m=b[1];if(i<m){var
A=a(c[24][15],t,z),H=o(j,f(z,0)[1]);f(A,0)[1]=H;var
n=[1,m,A]}else
if(m<i){var
B=a(c[24][15],t,l),I=o(f(l,0)[1],b);f(B,0)[1]=I;var
n=[1,i,B]}else{var
J=h(i,b),K=h(i,j),p=a(e[6],K,J),C=w(a(c[4],p,1),[0,d]),L=0;if(!(p<0)){var
k=L;for(;;){var
M=v(i,k,b),N=o(v(i,k,j),M);f(C,k)[1+k]=N;var
O=k+1|0;if(p!==k){var
k=O;continue}break}}var
n=[1,i,C]}var
y=n}var
r=y}return W(r)}function
L(e){if(0===e[0])return b(g[4],e[1]);var
f=a(c[24][15],L,e[2]);return j(c[24][17],g[12],d,f)}function
M(b,d){if(0===b[0])return[0,a(g[9],b[1],d)];var
e=b[2],f=b[1];function
h(a){return M(a,d)}return[1,f,a(c[24][15],h,e)]}function
Y(b){var
c=L(b);return a(g[1],c,d)?b:M(b,c)}function
Z(d){if(0===d[0])return 0;var
e=d[1],f=b(c[24][11],d[2]),g=[0,[0,e,0],a(c[22][68],Z,f)];return b(c[22][59],g)}function
_(g,i,e){if(1===e[0]){var
h=e[2],k=e[1];if(k===i){var
l=w(a(c[4],h.length-1,g),[0,d]),n=a(c[5],h.length-1,1),p=0;if(!(n<0)){var
b=p;for(;;){var
q=f(h,b)[1+b],o=a(c[4],b,g);f(l,o)[1+o]=q;var
r=b+1|0;if(n!==b){var
b=r;continue}break}}return[1,k,l]}}if(m(e,[0,d]))return[0,d];var
j=w(a(c[4],g,1),[0,d]);f(j,g)[1+g]=e;return[1,i,j]}function
n(e,b){if(0===e[0]){var
k=e[1];if(0===b[0])return[0,a(g[6],k,b[1])];var
l=b[2],m=b[1];if(a(g[1],k,d))return[0,d];var
p=function(a){return n(e,a)};return[1,m,a(c[24][15],p,l)]}var
h=e[2],f=e[1];if(0===b[0]){if(a(g[1],b[1],d))return[0,d];var
q=function(a){return n(a,b)};return[1,f,a(c[24][15],q,h)]}var
i=b[1],r=b[2];if(f<i){var
s=function(a){return n(e,a)};return[1,i,a(c[24][15],s,r)]}if(i<f){var
t=function(a){return n(a,b)};return[1,f,a(c[24][15],t,h)]}function
u(c,a){return _(c,f,n(a,b))}var
v=a(c[24][16],u,h);return j(c[24][17],o,[0,d],v)}function
al(m,e){if(0===e[0])return[0,d];var
g=e[2],i=e[1];if(i===m){var
h=a(c[5],g.length-1,1);if(1===h)return f(g,1)[2];var
j=w(h,[0,d]),k=a(c[5],h,1),o=0;if(!(k<0)){var
b=o;for(;;){var
l=a(c[4],b,1),p=f(g,l)[1+l],q=n([0,G(a(c[4],b,1))],p);f(j,b)[1+b]=q;var
r=b+1|0;if(k!==b){var
b=r;continue}break}}return[1,i,j]}return[0,d]}function
s(d){if(0===d[0])return[0,b(g[8],d[1])];var
e=d[1];return[1,e,a(c[24][15],s,d[2])]}function
N(b,a){return o(b,s(a))}function
x(d,b){return 0===b?U:n(d,x(d,a(c[5],b,1)))}function
l(b,a){return n(b,a)}function
O(b,a){return N(b,a)}function
y(b,a){return x(b,a)}function
p(b,a){return v(b,h(b,a),a)}function
am(b,a){return v(b,0,a)}function
A(b,a){var
c=h(b,a),d=x(V(b),c);return N(a,n(p(b,a),d))}function
P(c){var
a=c;for(;;){var
b=r(a);if(0<b){var
a=p(b,a);continue}if(0===a[0])return a[1];throw[0,H,br]}}function
an(c){var
b=Y(c),e=P(b);return a(g[3],d,e)?b:s(b)}var
$=[0,1];function
ao(b){var
a=b;for(;;){if(0===a[0])return a[1];var
a=f(a[2],0)[1];continue}}function
aa(d){if(b(c[3],$)){var
f=b(e[22],d);return a(e[17],bs,f)}if(3<d){var
g=a(c[5],d,4),h=a(c[4],g,97),i=b(aF[1],h);return a(c[20][1],1,i)}var
j=a(c[4],d,119),k=b(aF[1],j);return a(c[20][1],1,k)}var
i=[0,0];function
Q(n){if(0<b(c[3],i)){if(0===n[0]){var
o=n[1],u=b(c[3],i);i[1]=a(c[5],u,1);if(a(g[3],d,o))return b(g[15],o);var
v=b(g[15],o),w=a(e[17],v,bt);return a(e[17],bu,w)}var
p=n[2],m=aa(n[1]),j=[0,bv],h=[0,bw],r=Q(f(p,0)[1]);if(1-a(c[20][34],r,bx))j[1]=r;var
q=[0,0],s=a(c[5],p.length-1,1);if(!(s<1)){var
l=s;for(;;){if(0<=b(c[3],i)){var
k=Q(f(p,l)[1+l]);h[1]=bz;if(1===l){if(1-a(c[20][34],k,bA)){var
z=b(c[3],i);i[1]=a(c[5],z,1);if(a(c[20][34],k,bB))h[1]=m;else
if(a(c[20][22],k,43)){var
I=a(e[17],bD,m),J=a(e[17],k,I);h[1]=a(e[17],bE,J)}else{var
K=a(e[17],bF,m);h[1]=a(e[17],k,K)}}}else
if(1-a(c[20][34],k,bG)){var
L=b(c[3],i);i[1]=a(c[5],L,1);if(a(c[20][34],k,bH)){var
M=b(e[22],l),N=a(e[17],bI,M);h[1]=a(e[17],m,N)}else
if(a(c[20][22],k,43)){var
O=b(e[22],l),P=a(e[17],bJ,O),R=a(e[17],m,P),S=a(e[17],bK,R),T=a(e[17],k,S);h[1]=a(e[17],bL,T)}else{var
U=b(e[22],l),V=a(e[17],bM,U),W=a(e[17],m,V),X=a(e[17],bN,W);h[1]=a(e[17],k,X)}}var
A=b(c[3],h),t=1-b(c[20][40],A),B=t?1-b(c[3],q):t;if(B){var
C=b(c[3],i);i[1]=a(c[5],C,1);var
D=b(c[3],j);if(b(c[20][40],D))j[1]=b(c[3],h);else{var
F=b(c[3],h),G=a(e[17],bC,F),H=b(c[3],j);j[1]=a(e[17],H,G)}}}else{h[1]=bO;if(1-b(c[3],q)){var
Y=b(c[3],h),Z=a(e[17],bP,Y),_=b(c[3],j);j[1]=a(e[17],_,Z)}q[1]=1}var
E=l-1|0;if(1!==l){var
l=E;continue}break}}var
x=b(c[3],j);if(b(c[20][40],x)){var
y=b(c[3],i);i[1]=a(c[5],y,1);j[1]=by}return b(c[3],j)}return bQ}function
u(a){i[1]=20;return Q(a)}function
ap(b){var
c=u(b);return a(aD[bh],bR,c)}function
ab(f){var
d=[0,bS];function
g(f){var
g=u(f),h=a(e[17],g,bT),i=b(c[3],d);d[1]=a(e[17],i,h);return 0}a(c[24][13],g,f);var
h=b(c[3],d);a(e[17],h,bU);return 0}function
aq(a){return ab(b(c[24][12],a))}function
E(n,j,f){if(0===f){if(0===n[0]){var
s=n[1];if(0===j[0]){var
t=j[1],y=a(g[10],s,t);return a(g[1],y,d)?[0,[0,a(g[9],s,t)],k]:b(e[3],bV)}}throw[0,H,bW]}var
u=h(f,j),z=p(f,j),i=[0,n],r=[0,k],v=[0,1],B=A(f,j);for(;;){if(b(c[3],v))if(!m(b(c[3],i),k)){var
w=h(f,b(c[3],i));if(w<u)v[1]=0;else{var
D=p(f,b(c[3],i)),E=A(f,b(c[3],i)),F=q(D,z,a(c[5],f,1)),x=l(F,J(f,a(c[5],w,u)));r[1]=o(b(c[3],r),x);i[1]=O(E,l(x,B))}continue}var
C=b(c[3],i);return[0,b(c[3],r),C]}}function
q(f,d,c){var
g=E(f,d,c),h=g[2],i=g[1];if(m(h,k))return i;var
j=b(e[22],c),l=a(e[17],j,bX),n=a(e[17],bY,l),o=a(e[17],bZ,n),p=u(h),q=a(e[17],p,o),r=a(e[17],b0,q),s=a(e[17],b1,r),t=u(d),v=a(e[17],t,s),w=a(e[17],b2,v),x=a(e[17],b3,w),y=u(f),z=a(e[17],y,x),A=a(e[17],b4,z),B=a(e[17],b5,A);return b(e[3],B)}function
ar(c,b){var
d=r(b),f=r(c);return q(c,b,a(e[6],f,d))}function
as(d,b){var
g=r(b),i=r(d),f=a(e[6],i,g);try{var
j=h(f,b),k=h(f,d),l=a(c[4],1,k),m=a(c[5],l,j);q(n(d,x([0,P(b)],m)),b,f);var
o=1;return o}catch(a){a=z(a);if(a[1]===ak)return 0;throw a}}function
at(g,e,d){if(0===e[0])return[0,k,e,1,g];if(d===e[1]){var
i=[0,0],f=[0,g],j=p(d,e),q=A(d,e),m=[0,k],r=h(d,e);for(;;){var
s=h(d,e);if(s<=h(d,b(c[3],f))){var
t=h(d,b(c[3],f)),u=p(d,b(c[3],f)),v=A(d,b(c[3],f)),n=l(u,J(d,a(c[5],t,r))),w=l(n,q);f[1]=O(l(j,v),w);m[1]=o(l(j,b(c[3],m)),n);var
x=b(c[3],i);i[1]=a(c[4],x,1);continue}var
y=b(c[3],m),z=b(c[3],i);return[0,b(c[3],f),j,z,y]}}return[0,k,e,1,g]}function
C(g,e,b,d){if(1===b[0]){var
h=b[2];if(d===b[1]){var
i=function(e,b){return B(e,b,a(c[5],d,1))};return j(c[24][17],i,e,h)}}var
f=a(c[5],d,1);return g<50?S(g+1|0,e,b,f):ax(S,[0,e,b,f])}function
S(j,i,f,d){if(0===i[0]){var
p=i[1];if(0===f[0]){var
r=b(g[4],f[1]),s=b(g[4],p);return[0,a(g[12],s,r)]}}if(m(i,k))return f;if(m(f,k))return i;if(0===h(d,f))return j<50?C(j+1|0,f,i,d):ax(C,[0,f,i,d]);if(0===h(d,i))return j<50?C(j+1|0,i,f,d):ax(C,[0,i,f,d]);var
t=F(i,d),u=F(f,d),n=B(t,u,a(c[5],d,1));aC(b(e[22],d));var
v=q(i,n,d),o=ad(v,q(f,n,d),d);return l(n,q(o,F(o,d),d))}function
av(a,b,c){return a6(C(0,a,b,c))}function
B(a,b,c){return a6(S(0,a,b,c))}function
ac(c,b,a){return B(c,b,a)}function
au(c,b){var
d=r(b),f=r(c);return ac(c,b,a(e[6],f,d))}function
F(b,d){if(1===b[0]){var
e=b[2];if(b[1]===d){var
f=function(e,b){return B(e,b,a(c[5],d,1))};return j(c[24][17],f,k,e)}}return b}function
ae(w,v,u,t,r,d){var
f=w,b=v,e=u,n=t,j=r;for(;;){if(m(b,k))return f;var
o=h(d,b),g=p(d,b),i=a(c[5],j,o),x=s(b),z=a(c[4],i,1),A=E(l(y(s(g),z),f),x,d)[2],B=af(g,e,i,d),f=b,b=q(A,l(n,y(e,i)),d),e=B,n=g,j=o;continue}}function
ad(o,n,d){var
e=o,b=n;for(;;){if(m(b,k))return e;var
i=h(d,e),f=h(d,b);if(i<f){var
u=b,b=e,e=u;continue}var
j=a(c[5],i,f),g=p(d,b),q=s(b),r=a(c[4],j,1),t=E(l(y(s(g),r),e),q,d)[2];return ae(b,t,y(g,j),g,f,d)}}function
af(f,j,i,h){var
d=[0,f],g=a(c[5],i,1),k=1;if(!(g<1)){var
e=k;for(;;){d[1]=q(l(b(c[3],d),f),j,h);var
m=e+1|0;if(g!==e){var
e=m;continue}break}}return b(c[3],d)}function
R(d){if(0===d[0])return b(g[13],d[1]);var
e=d[2],f=0;function
h(d,b){var
e=R(d);return a(c[4],b,e)}return j(c[24][18],h,e,f)}return[0,I,V,J,ag,ai,r,K,aj,m,W,h,X,t,v,o,L,M,Y,Z,ah,_,n,al,s,N,x,l,O,y,p,am,A,P,an,ao,$,aa,i,u,ap,ab,aq,E,q,ar,as,at,au,ac,F,av,B,ad,ae,af,R,b(D[25],[0,m,R])]}O(166,[0,aG],"Nsatz_plugin__Polynom");var
al=[bi,b6,a7(0)],A=[0,0];function
b7(a){return a}function
b8(a){return a}function
y(a){return a.length-1-1|0}function
am(a){return f(a,0)[1]}function
aH(c,d){var
h=y(c);if(A[1]){var
e=[0,0],a=[0,1];for(;;){if(0===e[1])if(a[1]<=h){var
i=a[1],n=f(d,i)[1+i],j=a[1];e[1]=ay(f(c,j)[1+j],n);a[1]=a[1]+1|0;continue}return e[1]}}var
o=f(d,0)[1],k=ay(f(c,0)[1],o);if(0===k){var
g=[0,0],b=[0,h];for(;;){if(0===g[1])if(1<=b[1]){var
l=b[1],p=f(d,l)[1+l],m=b[1];g[1]=-ay(f(c,m)[1+m],p)|0;b[1]=b[1]-1|0;continue}return g[1]}}return k}function
ac(c,e){var
b=y(c),d=w(b+1|0,0),g=0;if(!(b<0)){var
a=g;for(;;){var
h=f(e,a)[1+a],i=f(c,a)[1+a]-h|0;f(d,a)[1+a]=i;var
j=a+1|0;if(b!==a){var
a=j;continue}break}}return d}function
aI(c,g){var
b=[0,1],a=[0,0],h=y(c);for(;;){if(b[1])if(a[1]<=h){var
d=a[1],i=f(g,d)[1+d],e=a[1];b[1]=i<=f(c,e)[1+e]?1:0;a[1]=a[1]+1|0;continue}return b[1]}}function
an(a){var
c=y(a);f(a,0)[1]=0;var
d=1;if(!(c<1)){var
b=d;for(;;){var
e=f(a,0)[1];a[1]=f(a,b)[1+b]+e|0;var
g=b+1|0;if(c!==b){var
b=g;continue}break}}return a}function
ad(d,h){var
c=y(d),g=w(c+1|0,0),i=1;if(!(c<1)){var
b=i;for(;;){var
j=f(h,b)[1+b],k=f(d,b)[1+b],l=a(e[6],k,j);f(g,b)[1+b]=l;var
m=b+1|0;if(c!==b){var
b=m;continue}break}}return an(g)}function
aJ(a){return an(w(a+1|0,0))}function
aL(d){var
Q=b(d[1],b9),t=b(d[1],b_);function
R(a){return a}function
S(c,b){var
f=b[2],g=c[2],e=a(d[9],c[1],b[1]),h=e?az(g,f):e;return h}var
B=b(c[22][51],S),T=[0,B,function(c){function
e(a){return a[1]}var
f=a(h[17],e,c);function
g(a){return a[2]}var
i=a(h[17],g,c),k=b(D[27],i);function
l(c,a){return(c*17|0)+b(d[56],a)|0}return j(h[20],l,k,f)}],U=b(D[25],T);function
C(f,d){try{var
c=a(h[7],f,d);return c}catch(c){c=z(c);if(c[1]===ak){var
g=b(e[22],d),i=a(e[17],cb,g),k=function(c,b){var
d=a(e[17],cd,b);return a(e[17],c,d)},l=j(h[20],k,cc,f);return a(e[17],l,i)}throw c}}function
o(h,c){var
k=h[1];function
g(j,i){var
d=[0,0],l=j.length-1-1|0,m=1;if(!(l<1)){var
c=m;for(;;){var
t=f(j,c)[1+c],h=b(e[22],t);if(Z(h,cj))if(Z(h,ck)){var
o=a(e[17],cl,h),p=C(k,c-1|0),q=[0,a(e[17],p,o),0];d[1]=a(e[26],d[1],q)}else{var
s=[0,C(k,c-1|0),0];d[1]=a(e[26],d[1],s)}var
r=c+1|0;if(l!==c){var
c=r;continue}break}}var
g=d[1];if(g){if(i)return a(ao[7],ce,g);var
n=a(ao[7],cf,g);return a(e[17],cg,n)}return i?ch:ci}function
l(i,k){var
z=i?0:1;if(z)return k?cv:cw;var
A=0,B=i?i[2]:b(e[3],cy),C=l(B,A),D=a(e[17],cx,C),m=i?i[1]:b(e[3],cz),h=m[2],n=b(d[39],m[1]),o=a(e[17],n,b$),c=a(e[17],ca,o);if(Z(c,cm))if(Z(c,cn))if(Z(c,co))if(45===p.caml_string_get(c,0))var
q=g(h,0),r=j(ao[4],c,1,p.caml_ml_string_length(c)-1|0),s=a(e[17],r,q),f=a(e[17],cp,s);else
if(0===k)var
t=g(h,0),u=a(e[17],c,t),f=a(e[17],cq,u);else
var
v=g(h,0),f=a(e[17],c,v);else
if(0===k)var
w=g(h,1),f=a(e[17],cr,w);else
var
f=g(h,1);else
var
f=cs;else
var
x=g(h,1),y=a(e[17],ct,x),f=a(e[17],cu,y);return a(e[17],f,D)}return l(c,1)}function
u(d,c){if(10<b(h[1],c))var
g=b(h[1],c),i=b(e[22],g),j=a(e[17],i,cA),k=a(e[17],cB,j),l=o(d,[0,b(h[5],c),0]),f=a(e[17],l,k);else
var
f=o(d,c);return f}var
V=0;function
k(b,a){return[0,[0,a,aJ(b)],0]}function
g(o,n){var
f=o,e=n,c=0;for(;;){if(f){if(e){var
i=e[2],j=e[1],k=f[2],g=f[1],l=aH(g[2],j[2]);if(0<l){var
f=k,c=[0,g,c];continue}if(0<=l){var
m=a(d[15],g[1],j[1]);if(a(d[9],m,Q)){var
f=k,e=i;continue}var
f=k,e=i,c=[0,[0,m,g[2]],c];continue}var
e=i,c=[0,j,c];continue}return a(h[12],c,f)}return e?a(h[12],c,e):b(h[9],c)}}function
l(m,g,b){function
c(h){var
n=h[2],o=h[1],c=y(g),e=w(c+1|0,0),i=0;if(!(c<0)){var
b=i;for(;;){var
j=f(n,b)[1+b],k=f(g,b)[1+b]+j|0;f(e,b)[1+b]=k;var
l=b+1|0;if(c!==b){var
b=l;continue}break}}return[0,a(d[22],m,o),e]}return a(ap[68],c,b)}function
m(a){return b(d[1],[0,a])}function
W(d,b){var
a=w(d+1|0,0);f(a,b)[1+b]=1;var
c=an(a);return[0,[0,m(1),c],0]}function
X(a){function
c(a){if(a){var
e=a[1],f=e[2],g=e[1],h=c(a[2]);return[0,[0,b(d[24],g),f],h]}return 0}return c(a)}function
n(f,b){function
c(b){if(b){var
e=b[1],g=e[2],h=e[1],i=c(b[2]);return[0,[0,a(d[22],f,h),g],i]}return 0}return c(b)}function
v(e,d){var
a=e,b=0;for(;;){if(a){var
c=a[1],f=a[2],a=f,b=g(l(c[1],c[2],d),b);continue}return b}}function
Y(a,c){if(a){if(0===c)return[0,[0,t,aJ(y(b(h[5],a)[2]))],0];var
d=function(b,a){if(1===a)return b;var
c=d(b,a/2|0),e=v(c,c);return 0===(a%2|0)?e:v(b,e)};return d(a,c)}return 0}function
E(c,b){return a(d[48],c,b)}function
i(a){return b(h[5],a[1])[2]}function
F(b,e){b[3]=b[3]+1|0;if(b[4].length-1<=b[3])b[4]=a(cC[5],b[4],w(b[3],aq));var
c=[0,e,b[3]],d=b[3];f(b[4],d)[1+d]=c;return c}function
_(e,d){var
a=d;for(;;){if(a){var
c=a[1],f=a[2];if(0===aI(e,b(h[5],c[1])[2])){var
a=f;continue}return c}return aq}}function
$(d,c){var
b=d[1];if(b)return a(D[6],b[1],c);throw ae}function
aa(d,c,b){var
a=d[1];return a?j(D[5],a[1],c,b):0}function
G(d,c,e){try{var
a=$(d,c);return a}catch(a){a=z(a);if(a===ae){var
b=_(c,e)[1];return b?(aa(d,c,b),b):b}throw a}}function
q(c,b){return a(d[45],c,b)}function
H(v,e,u){function
f(c){if(c){var
h=c[1],i=h[2],j=h[1],w=c[2],e=G(v,i,u);if(e){var
k=e[1],m=k[1],x=e[2],y=k[2],o=E(j,m),p=q(m,o),z=q(j,o),A=b(d[24],z),s=l(A,ac(i,y),x),r=f(g(n(p,w),s)),B=r[2];return[0,a(d[22],p,r[1]),B]}return[0,t,c]}return[0,t,0]}var
c=f(e);return[0,c[1],c[2]]}function
ab(d,c,b){try{var
e=a(D[6],d[2],[0,c[2],b[2]]);return e}catch(a){a=z(a);if(a===ae)return 0;throw a}}function
J(d,c,b,a){return j(D[5],d[2],[0,c[2],b[2]],a)}function
K(o,n,e,i){function
f(a){if(a){var
h=a[1],i=h[2],p=a[2],r=h[1],c=G(o,i,e);if(c){var
j=c[1],s=c[2],t=j[2],u=q(r,j[1]),k=b(d[24],u),m=ac(i,t),n=f(g(p,l(k,m,s)));return[0,[0,[0,k,m,c],n[1]],n[2]]}return[0,0,a]}return cD}var
c=f(n),p=c[2],r=c[1];function
s(b,e){function
c(c,b){var
d=b[2],f=b[1];if(a(B,b[3],e[1])){var
h=m(1);return g(c,l(f,d,k(y(d),h)))}return c}return j(h[20],c,b,r)}return[0,j(h[23],s,i,e),p]}function
af(s,r){var
c=s[1],e=r[1],a=b(h[5],c)[2],i=b(h[5],e)[2],j=b(h[5],c)[1],n=b(h[5],e)[1],t=b(h[6],c),u=b(h[6],e),o=E(j,n),p=ad(a,i),v=ac(p,a),w=ac(p,i);function
f(c,a){var
e=q(j,o),f=l(b(d[24],e),w,a);return g(l(q(n,o),v,c),f)}var
x=f(t,u),z=m(1),A=f(k(y(a),z),0),B=m(1);return[0,x,A,f(0,k(y(a),B))]}function
ag(c,b){return a(e[26],b,[0,c,0])}var
ah=[0,function(b,a){return aH(a[2],b[2])}],r=b(aK[2],ah),L=r[1],ai=r[2],aj=r[3],ar=r[4],as=r[6];function
M(k,d,c){function
e(r,j){var
u=j[1],l=b(h[5],k[1])[2],s=b(h[5],u)[2],d=[0,1],c=[0,1],t=y(l);for(;;){if(d[1])if(c[1]<=t){var
m=c[1],n=0===f(l,m)[1+m]?1:0;if(n)var
o=n;else
var
q=c[1],o=0===f(s,q)[1+q]?1:0;d[1]=o;c[1]=c[1]+1|0;continue}if(d[1])return r;var
v=i(j),w=ad(i(k),v),e=j[2],g=k[2],x=p.caml_lessthan(g,e)?[0,g,e]:[0,e,g];return a(ai,[0,x,w],r)}}return j(h[20],e,c,d)}function
at(e,d){var
a=e,b=d;for(;;){if(a){var
c=a[2],f=a[1];if(c){var
a=c,b=M(f,c,b);continue}}return b}}function
au(j,b,k,m){var
e=b[2],g=b[1],c=g[2],d=g[1];function
l(a){var
h=a[2]!==d?1:0;if(h){var
k=a[2]!==c?1:0;if(k){var
l=aI(e,i(a));if(l){var
m=a[2]<c?1:0;if(m)var
g=m;else
var
p=i(a),g=1-az(e,ad(i(f(j[4],d)[1+d]),p));if(g){var
n=a[2]<d?1:0;if(n)var
b=n;else
var
o=i(a),b=1-az(e,ad(i(f(j[4],c)[1+c]),o))}else
var
b=g}else
var
b=l}else
var
b=k}else
var
b=h;return b}return a(h[28],l,k)}function
av(e,d){return x(function(g){var
a=0,c=j(as,function(b,a){return a+1|0},d,a),f=b(h[1],e);return j(I[4],cE,f,c)})}var
A=[bi,cF,a7(0)];function
N(t,l,f,r,c,D){var
w=H(l,t[1],c),p=w[2],E=w[1];x(function(c){var
b=u(f,p);return a(e[17],cG,b)});var
y=[0,p,a(d[22],t[2],E)];if(p)throw[0,A,y];s(cH);function
F(a){return 0}var
G=a(h[17],F,c),i=y[2],z=K(l,n(i,r),c,G),B=z[1],I=z[2];s(cI);x(function(c){var
b=o(f,I);return a(e[17],cJ,b)});x(function(k){function
b(c,b,a){return g(c,v(b,a[1]))}var
d=n(i,r),j=o(f,a8(h[25],b,d,B,c));return a(e[17],cK,j)});x(function(c){var
b=o(f,k(1,i));return a(e[17],cL,b)});var
q=0,j=b(h[9],c);for(;;){if(j){var
C=j[2],J=j[1],L=function(b){return function(a){return ab(l,b,a)}}(J),q=[0,a(h[17],L,C),q],j=C;continue}var
M=a(ap[109],D,q),N=function(a){return n(m(-1),a)};return[0,i,1,M,a(h[19],N,B)]}}function
O(a){return a?am(a[1][2]):-1}function
P(d,q,k,R,c,w){var
g=c[1],l=c[2];s(cM);var
i=d[1];if(i)b(D[2],i[1]);var
y=b(h[1],g);return function(T,S){var
k=T,g=S;for(;;){var
r=g[2],c=g[1];av(c,r);try{var
P=b(ar,r),Q=[0,[0,b(aj,r),P]],t=Q}catch(a){a=z(a);if(a!==aK[1])throw a;var
t=0,ac=a}if(t){var
B=t[1],i=B[2],C=B[1],D=C[1],l=D[2],m=D[1];if(au(d,[0,[0,m,l],C[2]],c,i)){s(cN);var
g=[0,c,i];continue}var
U=f(d[4],l)[1+l],v=af(f(d[4],m)[1+m],U),o=v[1],V=v[3],W=v[2];if(R)if(0!==o){var
ab=O(k[1]);if(ab<O(o)){s(cQ);var
g=[0,c,i];continue}}var
E=H(d,o,c),G=E[1];if(E[2]){var
X=function(c,d,e,f,g){return function(a){var
b=a[2]===d?f:a[2]===c?e:0;return n(g,b)}}(l,m,V,W,G),Y=a(h[17],X,c),I=K(d,n(G,o),c,Y),Z=I[1],p=F(d,I[2]),_=function(c){return function(b,a){return J(d,c,a,b)}}(p);j(h[22],_,Z,c);x(function(c){return function(d){var
b=u(q,c[1]);return a(e[17],cO,b)}}(p));var
L=ag(p,c);try{var
aa=N(k,d,q,w,L,y);return aa}catch(a){a=z(a);if(a[1]===A){var
$=a[2],k=$,g=[0,L,M(p,c,i)];continue}throw a}}s(cP);var
g=[0,c,i];continue}return N(k,d,q,w,c,y)}}(k,[0,g,l])}function
aw(b){if(b){var
c=b[2],d=am(b[1][2]),e=function(a){return am(a[2])===d?1:0};return a(h[27],e,c)}return 1}return[0,R,k,V,W,B,g,X,v,Y,function(c,n,g,b){var
d=[0,0,a(D[1],0,51),0,w(1000,aq)],i=a(h[27],aw,[0,b,g]);if(i)s(cR);x(function(f){var
d=u(c,b);return a(e[17],cS,d)});x(function(f){function
b(d,b){var
f=u(c,b),g=a(e[17],f,cU);return a(e[17],d,g)}var
d=j(h[20],b,cT,g);return a(e[17],cV,d)});function
o(a){return F(d,a)}var
f=a(h[17],o,g);function
p(a){return J(d,a,a,k(n,m(1)))}a(h[15],p,f);var
q=[0,b,t];try{var
y=P(d,c,q,i,[0,f,L],b),l=y}catch(a){a=z(a);if(a[1]!==A)throw a;var
r=a[2];try{var
v=P(d,c,r,i,[0,f,at(f,L)],b)}catch(a){a=z(a);if(a[1]===A)throw al;throw a}var
l=v}s(cW);return l},U]}var
J=[0,b7,b8];O(174,[0,J,aL,al,A],"Nsatz_plugin__Ideal");var
cX=b(v[36],0),aM=k[52],aN=v[24],aO=v[27],aP=v[16],cY=k[51],cZ=v[25],c0=v[4],c1=v[5],c2=v[10],c3=v[8],c4=v[3],c5=v[15],c6=v[33];function
c7(a){try{var
c=b(v[38],a);return c}catch(a){a=z(a);if(a[1]===ak)return 1;throw a}}var
c8=v[19];function
c9(e,d){var
c=e,b=d;for(;;){if(a(aN,b,cX))return c;if(a(aO,c,b)){var
g=b,b=c,c=g;continue}var
f=a(aP,c,b),c=b,b=f;continue}}function
aQ(b){return a(k[32],b,B)?0:[0,b]}function
aR(a){var
b=a[2],c=a[1];return 1===b?c:[6,c,b]}function
as(a){var
b=a[1];if(typeof
b==="number")return a[2];var
c=a[2];return typeof
c==="number"?b:[3,b,c]}function
aS(c){var
b=c[1];if(typeof
b==="number")return 0;var
d=c[2];if(typeof
d==="number")return 0;else
if(0===d[0])if(a(k[32],d[1],K))return b;if(typeof
b!=="number"&&0===b[0]){var
e=c[2];if(a(k[32],b[1],K))return e}return[5,b,c[2]]}function
t(a){return[l,function(d){var
c=b(aT[2],a);return b(aU[15],c)}]}var
at=t(c_),L=t(c$),R=t(da),S=t(db),T=t(dc),U=t(dd),V=t(de),W=t(df),aV=t(dg),aW=t(dh),aX=t(di),g=t(dj),af=t(dk),X=t(dl),dn=t(dm),dq=t(dp),Y=t(dr),ag=t(ds),ah=t(dt),dv=t(du);function
q(a,e){var
f=b(c[24][12],e),d=n(a),g=o===d?a[1]:l===d?b(m[2],a):a;return b(u[16],[0,g,f])}function
au(f){var
a=n(g),c=0,d=0,e=o===a?g[1]:l===a?b(m[2],g):g;return q(aV,[0,q(at,[0,e,d]),c])}function
M(c){if(a(k[26],c,K)){var
d=n(ag);return o===d?ag[1]:l===d?b(m[2],ag):ag}var
e=a(k[12],c,Q);return a(k[26],e,B)?q(Y,[0,M(a(k[11],c,Q)),0]):q(dq,[0,M(a(k[11],c,Q)),0])}function
aY(c){if(a(k[26],c,B)){var
d=n(af);return o===d?af[1]:l===d?b(m[2],af):af}return a(k[28],c,B)?q(X,[0,M(c),0]):q(dn,[0,M(a(k[4],dw,c)),0])}function
E(A){var
c=A;for(;;)if(typeof
c==="number"){var
c=[0,B];continue}else
switch(c[0]){case
0:var
x=b(k[54],c[1]),d=b(ar[5],x),y=b(ar[3],d);b(k[51],y);var
z=b(ar[2],d),C=[0,aY(b(k[51],z)),0],f=n(g),D=o===f?g[1]:l===f?b(m[2],g):g;return q(L,[0,D,C]);case
1:var
F=[0,M(b(k[43],c[1])),0],h=n(g),G=o===h?g[1]:l===h?b(m[2],g):g;return q(R,[0,G,F]);case
2:var
H=[0,E(c[1]),0],i=n(g),I=o===i?g[1]:l===i?b(m[2],g):g;return q(V,[0,I,H]);case
3:var
J=c[1],N=[0,E(c[2]),0],O=[0,E(J),N],j=n(g),P=o===j?g[1]:l===j?b(m[2],g):g;return q(S,[0,P,O]);case
4:var
Q=c[1],X=[0,E(c[2]),0],Y=[0,E(Q),X],p=n(g),Z=o===p?g[1]:l===p?b(m[2],g):g;return q(T,[0,Z,Y]);case
5:var
_=c[1],$=[0,E(c[2]),0],aa=[0,E(_),$],r=n(g),ab=o===r?g[1]:l===r?b(m[2],g):g;return q(U,[0,ab,aa]);default:var
s=c[2],ac=c[1];if(0===s){var
ad=[0,aY(K),0],t=n(g),ae=o===t?g[1]:l===t?b(m[2],g):g;return q(L,[0,ae,ad])}var
u=b(k[47],s),af=0;if(a(k[32],u,B))var
e=n(ah),v=o===e?ah[1]:l===e?b(m[2],ah):ah;else
var
v=q(dv,[0,M(u),0]);var
ag=[0,E(ac),[0,v,af]],w=n(g),ai=o===w?g[1]:l===w?b(m[2],g):g;return q(W,[0,ai,ag])}}function
N(g){var
c=b(u[30],g);if(9===c[0]){var
d=c[2];if(1===d.length-1){var
e=d[1],h=c[1],f=n(Y),i=o===f?Y[1]:l===f?b(m[2],Y):Y;if(a(u[81],h,i)){var
j=N(e);return a(k[6],Q,j)}var
p=N(e),q=a(k[6],Q,p);return a(k[1],K,q)}}return K}function
F(M){var
i=b(u[30],M);if(9===i[0]){var
d=i[2],x=d.length-1,c=i[1];if(2===x){var
j=d[2],y=n(R),O=o===y?R[1]:l===y?b(m[2],R):R;if(a(u[81],c,O)){var
P=N(j);return[1,b(k[40],P)]}var
z=n(L),Q=o===z?L[1]:l===z?b(m[2],L):L;if(a(u[81],c,Q)){var
h=b(u[30],j);if(9===h[0]){var
r=h[2];if(1===r.length-1){var
s=r[1],I=h[1],t=n(X),J=o===t?X[1]:l===t?b(m[2],X):X;if(a(u[81],I,J))var
p=N(s),g=1;else
var
K=N(s),p=a(k[4],B,K),g=1}else
var
g=0}else
var
g=0;if(!g)var
p=B;return[0,p]}var
A=n(V),Y=o===A?V[1]:l===A?b(m[2],V):V;return a(u[81],c,Y)?[2,F(j)]:0}if(3===x){var
e=d[2],f=d[3],C=n(S),Z=o===C?S[1]:l===C?b(m[2],S):S;if(a(u[81],c,Z)){var
_=F(f);return[3,F(e),_]}var
D=n(T),$=o===D?T[1]:l===D?b(m[2],T):T;if(a(u[81],c,$)){var
aa=F(f);return[4,F(e),aa]}var
E=n(U),ab=o===E?U[1]:l===E?b(m[2],U):U;if(a(u[81],c,ab)){var
ac=F(f);return[5,F(e),ac]}var
G=n(W),ad=o===G?W[1]:l===G?b(m[2],W):W;if(a(u[81],c,ad)){var
v=b(u[30],f);if(9===v[0]){var
w=v[2];if(1===w.length-1)var
H=N(w[1]),q=1;else
var
q=0}else
var
q=0;if(!q)var
H=B;var
ae=b(k[45],H);return[6,F(e),ae]}return 0}}return 0}function
aZ(e){var
c=b(u[30],e);if(9===c[0]){var
a=c[2],d=a.length-1-1|0;if(!(2<d>>>0))switch(d){case
0:return 0;case
1:break;default:var
f=a[2],g=aZ(a[3]);return[0,F(f),g]}}throw[0,H,dx]}function
dy(c,b){function
d(g,f){var
b=g,c=f;for(;;)if(typeof
b==="number")return c;else
switch(b[0]){case
0:return c;case
1:var
h=a9(b[1]);return a(e[6],c,h);case
2:var
b=b[1];continue;case
3:var
i=b[2],j=d(b[1],c),b=i,c=j;continue;case
4:var
k=b[2],l=d(b[1],c),b=k,c=l;continue;case
5:var
m=b[2],n=d(b[1],c),b=m,c=n;continue;default:var
b=b[1];continue}}return d(b,c)}var
a0=aG([0,aN,aO,cZ,c0,c1,c2,c3,c4,c5,aP,c8,c9,c7,aM,c6]),r=aL(a0);function
av(i,d){var
g=b(r[1],d);function
h(g){if(g){var
n=g[2],o=g[1],s=o[1],t=b(J[1],o[2]).length-1,p=a(c[5],t,1),u=function(i,m){var
h=i[2],e=i[1],g=b(J[1],m[2]),a=[0,0],n=1;if(!(p<1)){var
d=n;for(;;){if(0<f(g,d)[1+d])a[1]=d;var
r=d+1|0;if(p!==d){var
d=r;continue}break}}if(0===b(c[3],a))return[0,e,h];if(e<b(c[3],a)){var
j=b(c[3],a),o=f(g,j)[1+j];return[0,b(c[3],a),o]}if(b(c[3],a)===e){var
k=b(c[3],a);if(f(g,k)[1+k]<h){var
l=b(c[3],a),q=f(g,l)[1+l];return[0,b(c[3],a),q]}}return[0,e,h]},q=j(c[22][15],u,dz,g),i=q[2],d=q[1];if(0===d){var
l=function(a){if(0===a[0])return aQ(b(cY,a[1]));var
d=a[2],f=a[1];function
g(d,c,a){var
g=aR([0,[1,b(e[22],f)],d]);return as([0,a,aS([0,l(c),g])])}return j(c[24][47],g,d,0)},m=l(s);return b(c[22][48],n)?m:as([0,m,h(n)])}var
v=function(j,h){var
e=h[2],k=h[1],l=j[2],m=j[1];if(i<=f(b(J[1],e),d)[1+d]){var
n=b(J[1],e),g=b(c[24][8],n),o=f(g,d)[1+d];g[1+d]=a(c[5],o,i);return[0,[0,[0,k,b(J[2],g)],m],l]}return[0,m,[0,[0,k,e],l]]},r=j(c[22][15],v,dA,g),w=r[2],x=r[1],y=1===i?[1,b(e[22],d)]:aR([0,[1,b(e[22],d)],i]),z=h(b(c[22][9],w));return as([0,aS([0,y,h(b(c[22][9],x))]),z])}return aQ(b(k[43],dB))}return h(g)}function
a2(e,a){function
d(b,a){if(b){if(0===b[1]){var
c=b[2];if(a){var
e=a[1];return[0,e,d(c,a[2])]}throw[0,H,dG]}var
f=d(b[2],a);return[0,r[3],f]}return a}var
f=d(e,b(c[22][9],a));return b(c[22][9],f)}function
dY(as){var
u=aZ(as),e=j(c[22][15],dy,0,u);if(u){var
v=u[1];if(typeof
v==="number")var
h=0;else
if(0===v[0]){var
N=v[1];if(0===N[0]){var
y=u[2];if(y){var
C=y[1];if(typeof
C==="number")var
G=1;else
if(0===C[0]){var
O=C[1],P=N[1];if(0===O[0]){var
Q=O[1],V=y[2];if(7<P>>>0){var
W=b(ab[3],dI);j(a3[5],0,0,W)}else
switch(P){case
0:s(dQ);A[1]=0;break;case
1:s(dR);A[1]=0;break;case
2:s(dS);A[1]=1;break;case
3:s(dT);A[1]=1;break;case
4:s(dU);A[1]=0;break;case
5:s(dV);A[1]=0;break;case
6:s(dW);A[1]=1;break;default:s(dX);A[1]=1}var
X=function(b){var
d=a(c[4],b,1);return a(I[4],dJ,d)},Y=a(c[22][56],e,X),Z=[0,a(c[23],dK,Y)],_=function(c){function
d(c){if(typeof
c==="number")var
f=r[3];else
switch(c[0]){case
0:var
h=c[1];if(a(k[32],h,B))var
i=r[3];else
var
l=[0,b(aM,h)],i=a(r[2],e,l);var
f=i;break;case
1:var
g=a9(c[1]);if(g<=Q)var
m=b(a0[2],g),j=a(r[2],e,m);else
var
j=a(r[4],e,g);var
f=j;break;case
2:var
n=d(c[1]),f=b(r[7],n);break;case
3:var
o=c[1],p=d(c[2]),q=d(o),f=a(r[6],q,p);break;case
4:var
s=c[1],t=d(c[2]),u=b(r[7],t),v=d(s),f=a(r[6],v,u);break;case
5:var
w=c[1],x=d(c[2]),y=d(w),f=a(r[8],y,x);break;default:var
z=c[2],A=d(c[1]),f=a(r[9],A,z)}return f}return d(c)},D=a(c[22][68],_,V);if(D){var
$=D[1],aa=b(c[22][9],D[2]),K=b(r[11][1],12),U=function(b){try{var
c=a(r[11][7],K,b);return c}catch(a){a=z(a);if(a===ae){j(r[11][5],K,b,1);return 0}throw a}},L=function(b){if(b){var
c=b[1],d=L(b[2]),e=d[2],f=d[1];if(!a(r[5],c,r[3]))if(!U(c))return[0,[0,c,f],[0,0,e]];return[0,f,[0,1,e]]}return dF},M=L(aa),R=M[2],ac=M[1],T=b(a1[90],0),t=a8(r[10],Z,e,ac,$);x(function(d){var
c=b(a1[90],0)-T;return a(I[4],dE,c)});s(dL);var
ad=b(c[22][9],t[3]),F=[0,t[4],ad],i=b(c[22][1],F),p=w(i,0),J=function(b){if(!(i<=b))if(!f(p,b)[1+b]){f(p,b)[1+b]=1;var
d=a(c[22][7],F,b),e=function(f,e){var
d=1-a(r[5],e,r[3]);if(d){var
g=a(c[4],f,b);return J(a(c[4],g,1))}return d};return a(c[22][12],e,d)}return 0};J(0);var
S=function(b,d){function
e(d,j){var
g=a(c[4],b,d);if(i<=a(c[4],g,1))return 1;var
h=a(c[4],b,d),e=a(c[4],h,1);return f(p,e)[1+e]}return f(p,b)[1+b]?[0,a(c[22][63],e,d)]:0},d=a(ap[66],S,F);x(function(g){var
e=b(c[22][1],d),f=a(c[5],i,e);return a(I[4],dC,f)});x(function(f){var
e=b(c[22][1],d);return a(I[4],dD,e)});if(d){var
af=d[2],ag=a2(R,d[1]),ah=av(e,a(r[2],e,t[1])),ai=[6,0,t[2]],aj=b(c[22][9],af),ak=function(a){return a2(R,a)},al=a(c[22][68],ak,aj),am=function(a){return av(e,a)},an=b(c[22][68],am),ao=a(c[22][68],an,al),aq=function(a){return av(e,a)},ar=a(c[22][68],aq,ag);x(function(b){return a(I[4],dM,Q)});s(dN);var
aw=a(c[23],[0,[0,ah,[0,ai,ar]],0],ao),ax=function(b){return a(c[22][68],E,b)},ay=a(c[22][68],ax,aw),az=q(aW,[0,au(0),0]),aA=function(e,d){var
a=n(g),f=0,h=0,i=o===a?g[1]:l===a?b(m[2],g):g,k=q(aW,[0,q(at,[0,i,h]),f]);function
p(d,c){var
a=n(g),e=[0,d,[0,c,0]],f=0,h=o===a?g[1]:l===a?b(m[2],g):g;return q(aX,[0,q(at,[0,h,f]),e])}var
r=[0,j(c[22][16],p,e,k),[0,d,0]];return q(aX,[0,au(0),r])},aB=j(c[22][16],aA,ay,az);s(dZ);return aB}throw[0,H,dO]}throw[0,H,dP]}var
h=1,G=0}else
var
G=1;if(G)var
h=1}else
var
h=1}else
var
h=1}else
var
h=0}throw[0,H,dH]}function
d0(a){var
c=[0,q(aV,[0,au(0),0]),a],d=b(aT[2],d1),e=[0,b(aU[15],d),c],f=b(u[16],e),g=[0,b(a4[9],f),0];return b(d2[148],g)}function
a5(a){try{var
e=dY(a),c=e}catch(a){a=z(a);if(a!==al)throw a;var
d=b(ab[3],d3),c=j(a3[5],0,0,d)}return d0(c)}O(186,[0,a5],"Nsatz_plugin__Nsatz");b(d4[9],aw);var
d5=0;function
d6(a,c){return a5(b(a4[142][1],a))}var
d_=[0,[0,[0,d9,[1,[5,b(d8[16],d7[11])],0]],d6],d5];ec(ea[8],aw,d$,0,0,d_);O(191,[0,aw],"Nsatz_plugin__G_nsatz");return}
