(function(e5){"use strict";var
ae="*",bg="x",bn="plugins/nsatz/polynom.ml",a_="+",a9="@[%s@]",q=250,aI="(",L="setoid_ring",bd="+ ",bm="Init",a8="...",j=246,bf="[",H=132,bk=120,bl="h",aG=" ",af="1",aF="nsatz_compute",a7="print_pol dans dansideal",bi="]",bj=")*",S="plugins/nsatz/nsatz.ml",a6="nsatz_plugin",bb=258,bc="c",B="",ba="term computed\n",ap="^",K="Ring_polynom",I="0",a$="$lt",ag=136,bh=248,be="u",x="\n",u="CC",aH=1e3,r=e5.jsoo_runtime,f=r.caml_check_bound,ao=r.caml_equal,aE=r.caml_int_compare,a5=r.caml_int_of_string,e3=r.caml_list_of_js_array,w=r.caml_make_vect,b=r.caml_new_string,p=r.caml_obj_tag,R=r.caml_register_global,ac=r.caml_string_equal,ad=r.caml_string_notequal,a4=r.caml_trampoline,aD=r.caml_trampoline_return,z=r.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):r.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):r.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):r.caml_call_gen(a,[b,c,d])}function
e4(a,b,c,d,e){return a.length==4?a(b,c,d,e):r.caml_call_gen(a,[b,c,d,e])}var
n=r.caml_get_global_data(),D=[0,0],N=[0,1],V=[0,2],ax=[0,b(bm),[0,b("Datatypes"),0]],E=[0,b("Numbers"),[0,b("BinNums"),0]],an=b(a6),T=n.CErrors,aq=n.Array,g=n.List,d=n.Pervasives,ah=n.Flags,ai=n.Format,e=n.Util,J=n.Assert_failure,ar=n.Failure,aK=n.Char,C=n.Hashtbl,U=n.Not_found,at=n.String,m=n.Num,o=n.CamlinternalLazy,v=n.Term,t=n.Coqlib,a0=n.Unix,y=n.Big_int,au=n.Ratio,aB=n.Names,aC=n.Ltac_plugin,a2=n.Mltop,bu=b(x),br=[0,[18,[1,[0,0,b(B)]],[2,0,[12,10,[17,0,0]]]],b("@[%s\n@]")],bp=[0,[18,[1,[0,0,b(B)]],[2,0,[17,0,0]]],b(a9)],bB=[0,b(bn),363,43],b0=b(a8),bD=b(")"),bE=b(aI),bF=b(B),bG=b(B),bH=b(I),bY=b(a8),bZ=b(a_),bJ=b(B),bK=b(I),bL=b(af),bN=b(bj),bO=b(aI),bP=b(ae),bQ=b(I),bR=b(af),bS=b(ap),bT=b(ap),bU=b(bj),bV=b(aI),bW=b(ap),bX=b(ae),bM=b(a_),bI=b(I),b5=b("div_pol1"),b6=[0,b(bn),475,9],b7=b(x),b8=b("x:"),b9=b(x),b_=b("r:"),b$=b(x),ca=b("q:"),cb=b(x),cc=b("p:"),cd=b("div_pol:\n"),b3=b(x),b2=b("\n{ "),b4=b("}"),b1=[0,[18,[1,[0,0,b(B)]],[2,0,[17,0,0]]],b(a9)],bC=b(be),bA=b("non"),by=[0,0],bz=[0,1],c_=b(x),c6=b("homogeneous polynomials\n"),c7=b(x),c8=b("p: "),c9=b(B),c$=b("lp:\n"),da=b("computed\n"),c1=b(bc),c5=b(bl),c2=b(x),c3=b("\nnew polynomial: "),c4=b(I),c0=b("computation of the Groebner basis\n"),cQ=b(x),cR=b("remainder: "),cS=b("polynomial reduced to 0\n"),cT=b("r ok\n"),cU=b(x),cV=b("r: "),cW=b(x),cX=b("verif sum: "),cY=b(x),cZ=b("coefficient: "),cN=b(bi),cO=b(","),cP=b(bf),cM=[0,0,0],cJ=b(" terms"),cK=b(" + "),cI=b(a7),cH=b(a7),cs=b(I),ct=b(af),cu=b(ap),cn=b(ae),co=b(ae),cp=b(ae),cq=b(af),cr=b(B),cv=b("-1"),cw=b(I),cx=b(af),cA=b(bd),cB=b(B),cC=b(aG),cD=b("-"),cy=b("- "),cz=b(bd),cE=b(I),cF=b(B),cG=b(aG),cm=b(aG),ck=b(" i="),cl=b("lv= "),ci=b(bi),cj=b(bf),cf=[0,0],cg=[0,1],ch=[0,-1],ce=b("Ideal.NotInIdeal"),ed=[0,b(S),bb,9],eo=[0,0,0],eQ=b("nsatz cannot solve this problem"),eM=b("refl_equal"),eN=[0,b(bm),[0,b("Logic"),0]],eO=b(u),eK=b(ba),er=b("nsatz: bad parameter"),eB=b("computation without sugar\n"),eC=b("computation with sugar\n"),eD=b("ordre lexico computation without sugar\n"),eE=b("ordre lexico computation with sugar\n"),eF=b("computation without sugar, division by pairs\n"),eG=b("computation with sugar, division by pairs\n"),eH=b("ordre lexico computation without sugar, division by pairs\n"),eI=b("ordre lexico computation with sugar, division by pairs\n"),ez=b(B),eA=b(bg),es=e3([b("a"),b("b"),b(bc),b("d"),b("e"),b("f"),b("g"),b(bl),b("i"),b("j"),b("k"),b("l"),b("m"),b("n"),b("o"),b("p"),b("q"),b("r"),b("s"),b("t"),b(be),b("v"),b("w"),b(bg),b("y"),b("z")]),et=b("cert ok\n"),eu=b(x),ev=b("number of parameters: "),ew=b(ba),ex=[0,b(S),568,11],ey=[0,b(S),556,14],eq=[0,b(S),585,8],ep=[0,b(S),505,11],em=[0,[18,[1,[0,0,b(B)]],[8,0,[0,1,10],[0,3],[17,0,[11,b("s\n"),0]]]],b("@[%10.3f@]s\n")],en=b("time: "),ei=b(x),ej=b("useless spolynomials: "),ek=b(x),el=b("useful spolynomials: "),eh=[0,b(S),412,12],ef=[0,0,0],eg=b(I),ec=[0,0],d$=b("Npos"),ea=b(u),d9=b("N0"),d_=b(u),d7=b("xH"),d8=b(u),d5=b("xO"),d6=b(u),d2=b("xI"),d3=b(u),dZ=b("Zneg"),d0=b(u),dX=b("Zpos"),dY=b(u),dV=b("Z0"),dW=b(u),dT=b("Z"),dU=b(u),dR=b("cons"),dS=b(u),dP=b("nil"),dQ=b(u),dN=b("list"),dO=b(u),dK=b("PEpow"),dL=[0,b(L),[0,b(K),0]],dM=b(u),dH=b("PEopp"),dI=[0,b(L),[0,b(K),0]],dJ=b(u),dE=b("PEmul"),dF=[0,b(L),[0,b(K),0]],dG=b(u),dB=b("PEsub"),dC=[0,b(L),[0,b(K),0]],dD=b(u),dy=b("PEadd"),dz=[0,b(L),[0,b(K),0]],dA=b(u),dv=b("PEX"),dw=[0,b(L),[0,b(K),0]],dx=b(u),ds=b("PEc"),dt=[0,b(L),[0,b(K),0]],du=b(u),dp=b("PExpr"),dq=[0,b(L),[0,b(K),0]],dr=b(u),eY=b("Extension: cannot occur"),eT=b(aF),eV=b(a$),eW=b(aF),eR=b(a6),eZ=b(a$),e2=b(aF),cL=n.CList,eP=n.Tactics,eU=n.Loc;function
bo(b){return ah[24][1]?(a(ai[97],bp,b),c(d[46],d[24])):0}function
bq(b){return ah[24][1]?(a(ai[97],br,b),c(d[46],d[24])):0}function
bs(a){return 0}function
bt(b){if(ah[24][1]){var
e=a(d[16],b,bu);c(d[27],e);return c(d[46],d[24])}return 0}function
bv(b){return a(ah[49],d[34],b)}function
aJ(e,d,c){var
b=c;for(;;){if(b){var
f=b[2];if(a(e,d,b[1]))return 1;var
b=f;continue}return 0}}function
bw(e,d){var
b=[0,0];function
f(a){var
c=1-aJ(e,a,b[1]),d=c?(b[1]=[0,a,b[1]],0):c;return d}a(g[11],f,d);return c(g[6],b[1])}function
bx(k,f,o){function
p(a){return 1-c(f,a)}var
e=0,b=a(g[30],p,o);for(;;){if(b){var
h=b[2],i=b[1],j=[0,0],l=[0,0],q=function(h,d,i){return function(e){try{var
b=a(k,h,e),g=c(f,b)?(i[1]=1,0):(d[1]=[0,b,d[1]],0);return g}catch(a){a=z(a);if(c(T[21],a))return 0;throw a}}}(i,j,l);a(g[11],q,e);if(l[1]){var
b=h;continue}if(0===j[1]){var
m=[0,h],n=[0,0],r=function(j,g,h){return function(b){try{var
d=a(k,b,j),e=1-c(f,d),i=e?(g[1]=[0,d,g[1]],0):e;return i}catch(a){a=z(a);if(c(T[21],a)){h[1]=[0,b,h[1]];return 0}throw a}}}(i,m,n);a(g[11],r,e);var
s=m[1],e=c(g[6],[0,i,n[1]]),b=s;continue}var
b=a(d[22],j[1],h);continue}return e}}var
l=[0,bo,bq,bs,bt,bv,aJ,bw,bx,function(k,j,d,b,e){var
g=w(b.length-1,[0,d,0]);function
h(i,h){var
b=[0,h],d=[0,0];if(1-c(j,h)){var
l=function(f,e){try{for(;;){var
g=a(k,b[1],e);d[1]=[0,f,d[1]];b[1]=g;continue}}catch(a){a=z(a);if(c(T[21],a))return 0;throw a}};a(aq[14],l,e)}var
m=[0,b[1],d[1]];return f(g,i)[i+1]=m}a(aq[14],h,b);return[0,e,g]}];R(bh,l,"Nsatz_plugin.Utile");var
as=[0,function(g){function
H(a){return c(g[14],[0,a])}var
b=H(0),U=H(1);function
I(a){return[0,c(g[14],a)]}var
k=I(by),V=I(bz);function
W(a){return[1,a,[0,k,V]]}function
K(d,a){if(0===a)return[0,U];var
c=w(a+1|0,[0,b]);f(c,a)[a+1]=[0,U];return[1,d,c]}function
aj(a){return 0===a[0]?1:0}function
ak(a){return 0===a[0]?a[1]:c(d[2],bA)}function
al(c){return 0===c[0]?a(g[1],c[1],b)?1:0:0}function
s(a){return 0===a[0]?0:a[1]}function
L(b){if(0===b[0])return 0;var
c=b[2],f=b[1];function
g(c,b){var
e=L(c);return a(d[5],e,b)}return h(e[19][18],g,c,f)}function
am(b){var
c=0;function
f(c,b){var
e=L(c);return a(d[5],e,b)}return h(e[19][18],f,b,c)}function
n(c,b){if(0===c[0]){var
f=c[1];if(0===b[0])return a(g[1],f,b[1])}else{var
i=c[2],j=c[1];if(0!==b[0]){var
d=j===b[1]?1:0,k=b[2];return d?h(e[19][31],n,i,k):d}}return 0}function
X(d){if(0===d[0])return d;var
e=d[2],g=e.length-1-1|0,a=[0,g],k=d[1];for(;;){if(0<a[1]){var
h=a[1];if(n(f(e,h)[h+1],[0,b])){a[1]=a[1]-1|0;continue}}if(0<=a[1]){if(0===a[1])return f(e,0)[1];if(a[1]===g)return d;var
i=w(a[1]+1|0,[0,b]),j=a[1],l=0;if(!(j<0)){var
c=l;for(;;){var
m=f(e,c)[c+1];f(i,c)[c+1]=m;var
o=c+1|0;if(j!==c){var
c=o;continue}break}}return[1,k,i]}return[0,b]}}function
i(b,a){if(1===a[0]){var
c=a[2];if(a[1]===b)return c.length-1-1|0}return 0}function
Y(c){if(0===c[0])return 0;var
b=[0,0],f=c[2];function
g(e,c){var
f=e+Y(c)|0;b[1]=a(d[5],b[1],f);return 0}a(e[19][14],g,f);return b[1]}function
u(b){if(0===b[0])return[0,b[1]];var
c=b[1];return[1,c,a(e[19][15],u,b[2])]}function
x(e,c,a){if(1===a[0]){var
d=a[2];if(a[1]===e)return c<d.length-1?f(d,c)[c+1]:[0,b]}return 0===c?a:[0,b]}function
p(j,c){if(0===j[0]){var
D=j[1];if(0===c[0])var
q=[0,a(g[5],D,c[1])];else{var
s=c[2],E=c[1],t=a(e[19][15],u,s),F=p(j,f(s,0)[1]);f(t,0)[1]=F;var
q=[1,E,t]}var
r=q}else{var
l=j[2],h=j[1];if(0===c[0]){var
v=a(e[19][15],u,l),G=p(f(l,0)[1],c);f(v,0)[1]=G;var
y=[1,h,v]}else{var
z=c[2],m=c[1];if(h<m){var
A=a(e[19][15],u,z),H=p(j,f(z,0)[1]);f(A,0)[1]=H;var
n=[1,m,A]}else
if(m<h){var
B=a(e[19][15],u,l),I=p(f(l,0)[1],c);f(B,0)[1]=I;var
n=[1,h,B]}else{var
J=i(h,c),K=i(h,j),o=a(d[5],K,J),C=w(o+1|0,[0,b]),L=0;if(!(o<0)){var
k=L;for(;;){var
M=x(h,k,c),N=p(x(h,k,j),M);f(C,k)[k+1]=N;var
O=k+1|0;if(o!==k){var
k=O;continue}break}}var
n=[1,h,C]}var
y=n}var
r=y}return X(r)}function
M(d){if(0===d[0])return c(g[4],d[1]);var
f=a(e[19][15],M,d[2]);return h(e[19][17],g[12],b,f)}function
N(b,c){if(0===b[0])return[0,a(g[9],b[1],c)];var
d=b[2],f=b[1];function
h(a){return N(a,c)}return[1,f,a(e[19][15],h,d)]}function
Z(c){var
d=M(c);return a(g[1],d,b)?c:N(c,d)}function
_(b){if(0===b[0])return 0;var
d=b[1],f=c(e[19][11],b[2]),g=[0,[0,d,0],a(e[17][12],_,f)];return c(e[17][10],g)}function
$(d,g,c){if(1===c[0]){var
e=c[2],i=c[1];if(i===g){var
j=w(e.length-1+d|0,[0,b]),k=e.length-1-1|0,m=0;if(!(k<0)){var
a=m;for(;;){var
l=a+d|0,o=f(e,a)[a+1];f(j,l)[l+1]=o;var
p=a+1|0;if(k!==a){var
a=p;continue}break}}return[1,i,j]}}if(n(c,[0,b]))return[0,b];var
h=w(d+1|0,[0,b]);f(h,d)[d+1]=c;return[1,g,h]}function
o(d,c){if(0===d[0]){var
k=d[1];if(0===c[0])return[0,a(g[6],k,c[1])];var
l=c[2],m=c[1];if(a(g[1],k,b))return[0,b];var
n=function(a){return o(d,a)};return[1,m,a(e[19][15],n,l)]}var
i=d[2],f=d[1];if(0===c[0]){if(a(g[1],c[1],b))return[0,b];var
q=function(a){return o(a,c)};return[1,f,a(e[19][15],q,i)]}var
j=c[1],r=c[2];if(f<j){var
s=function(a){return o(d,a)};return[1,j,a(e[19][15],s,r)]}if(j<f){var
t=function(a){return o(a,c)};return[1,f,a(e[19][15],t,i)]}function
u(b,a){return $(b,f,o(a,c))}var
v=a(e[19][16],u,i);return h(e[19][17],p,[0,b],v)}function
an(k,c){if(0===c[0])return[0,b];var
d=c[2],g=c[1];if(g===k){var
e=d.length-1-1|0;if(1===e)return f(d,1)[2];var
h=w(e,[0,b]),i=e-1|0,l=0;if(!(i<0)){var
a=l;for(;;){var
j=a+1|0,m=f(d,j)[j+1],n=o([0,H(a+1|0)],m);f(h,a)[a+1]=n;var
p=a+1|0;if(i!==a){var
a=p;continue}break}}return[1,g,h]}return[0,b]}function
t(b){if(0===b[0])return[0,c(g[8],b[1])];var
d=b[1];return[1,d,a(e[19][15],t,b[2])]}function
O(b,a){return p(b,t(a))}function
y(b,a){return 0===a?V:o(b,y(b,a-1|0))}function
m(b,a){return o(b,a)}function
P(b,a){return O(b,a)}function
A(b,a){return y(b,a)}function
q(b,a){return x(b,i(b,a),a)}function
ao(b,a){return x(b,0,a)}function
B(b,a){var
c=i(b,a),d=y(W(b),c);return O(a,o(q(b,a),d))}function
Q(c){var
a=c;for(;;){var
b=s(a);if(0<b){var
a=q(b,a);continue}if(0===a[0])return a[1];throw[0,J,bB]}}function
ap(d){var
c=Z(d),e=Q(c);return a(g[3],b,e)?c:t(c)}var
aa=[0,1];function
aq(b){var
a=b;for(;;){if(0===a[0])return a[1];var
a=f(a[2],0)[1];continue}}function
ab(b){if(aa[1]){var
f=c(d[20],b);return a(d[16],bC,f)}if(3<b){var
g=c(aK[1],(b-4|0)+97|0);return a(e[15][1],1,g)}var
h=c(aK[1],b+119|0);return a(e[15][1],1,h)}var
j=[0,0];function
R(n){if(0<j[1]){if(0===n[0]){var
o=n[1];j[1]=j[1]-1|0;if(a(g[3],b,o))return c(g[15],o);var
u=c(g[15],o),v=a(d[16],u,bD);return a(d[16],bE,v)}var
p=n[2],m=ab(n[1]),i=[0,bF],h=[0,bG],r=R(f(p,0)[1]);if(1-ac(r,bH))i[1]=r;var
q=[0,0],s=p.length-1-1|0;if(!(s<1)){var
l=s;for(;;){if(0<=j[1]){var
k=R(f(p,l)[l+1]);h[1]=bJ;if(1===l){if(1-ac(k,bK)){j[1]=j[1]-1|0;if(ac(k,bL))h[1]=m;else
if(a(e[15][18],k,43)){var
z=a(d[16],bN,m),A=a(d[16],k,z);h[1]=a(d[16],bO,A)}else{var
B=a(d[16],bP,m);h[1]=a(d[16],k,B)}}}else
if(1-ac(k,bQ)){j[1]=j[1]-1|0;if(ac(k,bR)){var
C=c(d[20],l),D=a(d[16],bS,C);h[1]=a(d[16],m,D)}else
if(a(e[15][18],k,43)){var
E=c(d[20],l),F=a(d[16],bT,E),G=a(d[16],m,F),H=a(d[16],bU,G),I=a(d[16],k,H);h[1]=a(d[16],bV,I)}else{var
J=c(d[20],l),K=a(d[16],bW,J),L=a(d[16],m,K),M=a(d[16],bX,L);h[1]=a(d[16],k,M)}}var
t=1-c(e[15][32],h[1]),w=t?1-q[1]:t;if(w){j[1]=j[1]-1|0;if(c(e[15][32],i[1]))i[1]=h[1];else{var
y=a(d[16],bM,h[1]);i[1]=a(d[16],i[1],y)}}}else{h[1]=bY;if(1-q[1]){var
N=a(d[16],bZ,h[1]);i[1]=a(d[16],i[1],N)}q[1]=1}var
x=l-1|0;if(1!==l){var
l=x;continue}break}}if(c(e[15][32],i[1])){j[1]=j[1]-1|0;i[1]=bI}return i[1]}return b0}function
v(a){j[1]=20;return R(a)}function
as(b){var
c=v(b);return a(ai[97],b1,c)}function
ad(f){var
b=[0,b2];function
g(c){var
e=v(c),f=a(d[16],e,b3);b[1]=a(d[16],b[1],f);return 0}a(e[19][13],g,f);var
h=a(d[16],b[1],b4);return c(l[3],h)}function
at(a){return ad(c(e[19][12],a))}function
F(j,h,e){if(0===e){if(0===j[0]){var
o=j[1];if(0===h[0]){var
s=h[1],x=a(g[10],o,s);return a(g[1],x,b)?[0,[0,a(g[9],o,s)],k]:c(d[2],b5)}}throw[0,J,b6]}var
t=i(e,h),y=q(e,h),f=[0,j],l=[0,k],u=[0,1],z=B(e,h);for(;;){if(u[1])if(!n(f[1],k)){var
v=i(e,f[1]);if(v<t)u[1]=0;else{var
A=q(e,f[1]),C=B(e,f[1]),D=r(A,y,e-1|0),w=m(D,K(e,v-t|0));l[1]=p(l[1],w);f[1]=P(C,m(w,z))}continue}return[0,l[1],f[1]]}}function
r(f,e,b){var
g=F(f,e,b),h=g[2],i=g[1];if(n(h,k))return i;var
j=c(d[20],b),l=a(d[16],j,b7),m=a(d[16],b8,l),o=a(d[16],b9,m),p=v(h),q=a(d[16],p,o),r=a(d[16],b_,q),s=a(d[16],b$,r),t=v(e),u=a(d[16],t,s),w=a(d[16],ca,u),x=a(d[16],cb,w),y=v(f),z=a(d[16],y,x),A=a(d[16],cc,z),B=a(d[16],cd,A);return c(d[2],B)}function
au(c,b){var
e=s(b),f=s(c);return r(c,b,a(d[5],f,e))}function
av(c,b){var
f=s(b),g=s(c),e=a(d[5],g,f);try{var
h=i(e,b),j=(1+i(e,c)|0)-h|0;r(o(c,y([0,Q(b)],j)),b,e);var
k=1;return k}catch(a){a=z(a);if(a[1]===ar)return 0;throw a}}function
aw(d,b,a){if(0===b[0])return[0,k,b,1,d];if(a===b[1]){var
e=[0,0],c=[0,d],f=q(a,b),j=B(a,b),g=[0,k],l=i(a,b);for(;;){var
n=i(a,b);if(n<=i(a,c[1])){var
o=i(a,c[1]),r=q(a,c[1]),s=B(a,c[1]),h=m(r,K(a,o-l|0)),t=m(h,j);c[1]=P(m(f,s),t);g[1]=p(m(f,g[1]),h);e[1]=e[1]+1|0;continue}return[0,c[1],f,e[1],g[1]]}}return[0,k,b,1,d]}function
E(f,c,a,b){if(1===a[0]){var
g=a[2];if(b===a[1]){var
i=function(c,a){return D(c,a,b-1|0)};return h(e[19][17],i,c,g)}}var
d=b-1|0;return f<50?T(f+1|0,c,a,d):aD(T,[0,c,a,d])}function
T(h,f,e,b){if(0===f[0]){var
p=f[1];if(0===e[0]){var
q=c(g[4],e[1]),s=c(g[4],p);return[0,a(g[12],s,q)]}}if(n(f,k))return e;if(n(e,k))return f;if(0===i(b,e))return h<50?E(h+1|0,e,f,b):aD(E,[0,e,f,b]);if(0===i(b,f))return h<50?E(h+1|0,f,e,b):aD(E,[0,f,e,b]);var
t=G(f,b),j=D(t,G(e,b),b-1|0),u=c(d[20],b);c(l[1],u);var
v=r(f,j,b),o=af(v,r(e,j,b),b);return m(j,r(o,G(o,b),b))}function
ay(a,b,c){return a4(E(0,a,b,c))}function
D(a,b,c){return a4(T(0,a,b,c))}function
ae(c,b,a){return D(c,b,a)}function
ax(c,b){var
e=s(b),f=s(c);return ae(c,b,a(d[5],f,e))}function
G(a,b){if(1===a[0]){var
c=a[2];if(a[1]===b){var
d=function(c,a){return D(c,a,b-1|0)};return h(e[19][17],d,k,c)}}return a}function
ag(u,s,p,o,l,b){var
d=u,a=s,c=p,h=o,g=l;for(;;){if(n(a,k))return d;var
j=i(b,a),e=q(b,a),f=g-j|0,v=t(a),w=F(m(A(t(e),f+1|0),d),v,b)[2],x=ah(e,c,f,b),d=a,a=r(w,m(h,A(c,f)),b),c=x,h=e,g=j;continue}}function
af(j,h,b){var
c=j,a=h;for(;;){if(n(a,k))return c;var
f=i(b,c),d=i(b,a);if(f<d){var
p=a,a=c,c=p;continue}var
g=f-d|0,e=q(b,a),l=t(a),o=F(m(A(t(e),g+1|0),c),l,b)[2];return ag(a,o,A(e,g),e,d,b)}}function
ah(c,g,f,e){var
a=[0,c],d=f-1|0,h=1;if(!(d<1)){var
b=h;for(;;){a[1]=r(m(a[1],c),g,e);var
i=b+1|0;if(d!==b){var
b=i;continue}break}}return a[1]}function
S(a){if(0===a[0])return c(g[13],a[1]);var
b=a[2],d=0;function
f(b,a){return a+S(b)|0}return h(e[19][18],f,b,d)}return[0,I,W,K,aj,al,s,L,am,n,X,i,Y,u,x,p,M,N,Z,_,ak,$,o,an,t,O,y,m,P,A,q,ao,B,Q,ap,aq,aa,ab,j,v,as,ad,at,F,r,au,av,aw,ax,ae,G,ay,D,af,ag,ah,S,c(C[18],[0,n,S])]}];R(254,as,"Nsatz_plugin.Polynom");var
aj=[bh,ce,r.caml_fresh_oo_id(0)],aL=[0,0],A=[0,function(i){var
al=c(i[1],cf),m=c(i[1],cg),am=c(i[1],ch);function
j(a){return a.length-1-1|0}function
T(c,d){var
h=j(c);if(aL[1]){var
e=[0,0],a=[0,1];for(;;){if(0===e[1])if(a[1]<=h){var
i=a[1],o=f(d,i)[i+1],k=a[1];e[1]=aE(f(c,k)[k+1],o);a[1]=a[1]+1|0;continue}return e[1]}}var
p=f(d,0)[1],l=aE(f(c,0)[1],p);if(0===l){var
g=[0,0],b=[0,h];for(;;){if(0===g[1])if(1<=b[1]){var
m=b[1],q=f(d,m)[m+1],n=b[1];g[1]=-aE(f(c,n)[n+1],q)|0;b[1]=b[1]-1|0;continue}return g[1]}}return l}function
B(c,e){var
b=j(c),d=w(b+1|0,0),g=0;if(!(b<0)){var
a=g;for(;;){var
h=f(e,a)[a+1],i=f(c,a)[a+1]-h|0;f(d,a)[a+1]=i;var
k=a+1|0;if(b!==a){var
a=k;continue}break}}return d}function
V(c,g){var
b=[0,1],a=[0,0],h=j(c);for(;;){if(b[1])if(a[1]<=h){var
d=a[1],i=f(g,d)[d+1],e=a[1];b[1]=i<=f(c,e)[e+1]?1:0;a[1]=a[1]+1|0;continue}return b[1]}}function
M(a){var
c=j(a);f(a,0)[1]=0;var
d=1;if(!(c<1)){var
b=d;for(;;){var
e=f(a,0)[1];a[1]=f(a,b)[b+1]+e|0;var
g=b+1|0;if(c!==b){var
b=g;continue}break}}return a}function
D(e,h){var
c=j(e),g=w(c+1|0,0),i=1;if(!(c<1)){var
b=i;for(;;){var
k=f(h,b)[b+1],l=f(e,b)[b+1],m=a(d[5],l,k);f(g,b)[b+1]=m;var
n=b+1|0;if(c!==b){var
b=n;continue}break}}return M(g)}function
an(a){return a}function
ap(c,b){var
e=b[2],f=c[2],d=a(i[9],c[1],b[1]),g=d?ao(f,e):d;return g}var
N=c(e[17][92],ap),as=[0,N,function(b){function
d(a){return a[1]}var
e=a(g[13],d,b);function
f(a){return a[2]}var
j=a(g[13],f,b),k=c(C[20],j);function
l(b,a){return(b*17|0)+c(i[56],a)|0}return h(g[16],l,k,e)}],au=c(C[18],as);function
W(f,e){try{var
b=a(g[5],f,e);return b}catch(b){b=z(b);if(b[1]===ar){var
i=c(d[20],e),j=a(d[16],ck,i),k=function(c,b){var
e=a(d[16],cm,b);return a(d[16],c,e)},l=h(g[16],k,cl,f);return a(d[16],l,j)}throw b}}var
O=[0,0];function
x(b){function
g(j,i){var
e=[0,0],k=j.length-1-1|0,l=1;if(!(k<1)){var
b=l;for(;;){var
s=f(j,b)[b+1],h=c(d[20],s);if(ad(h,cs))if(ad(h,ct)){var
n=a(d[16],cu,h),o=W(O[1],b-1|0),p=[0,a(d[16],o,n),0];e[1]=a(d[22],e[1],p)}else{var
r=[0,W(O[1],b-1|0),0];e[1]=a(d[22],e[1],r)}var
q=b+1|0;if(k!==b){var
b=q;continue}break}}var
g=e[1];if(g){if(i)return a(at[7],cn,g);var
m=a(at[7],co,g);return a(d[16],cp,m)}return i?cq:cr}function
l(j,k){var
z=j?0:1;if(z)return k?cE:cF;var
A=0,B=j?j[2]:c(d[2],cH),C=l(B,A),D=a(d[16],cG,C),m=j?j[1]:c(d[2],cI),f=m[2],n=c(i[39],m[1]),o=a(d[16],n,ci),b=a(d[16],cj,o);if(ad(b,cv))if(ad(b,cw))if(ad(b,cx))if(45===r.caml_string_get(b,0))var
p=g(f,0),q=h(at[4],b,1,r.caml_ml_string_length(b)-1|0),s=a(d[16],q,p),e=a(d[16],cy,s);else
if(0===k)var
t=g(f,0),u=a(d[16],b,t),e=a(d[16],cz,u);else
var
v=g(f,0),e=a(d[16],b,v);else
if(0===k)var
w=g(f,1),e=a(d[16],cA,w);else
var
e=g(f,1);else
var
e=cB;else
var
x=g(f,1),y=a(d[16],cC,x),e=a(d[16],cD,y);return a(d[16],e,D)}return l(b,1)}var
P=[0,d[7]];function
E(b){P[1]=10;var
f=P[1];if(f<c(g[1],b))var
h=c(g[1],b),i=c(d[20],h),j=a(d[16],i,cJ),k=a(d[16],cK,j),l=x([0,c(g[3],b),0]),e=a(d[16],l,k);else
var
e=x(b);P[1]=d[7];return e}var
av=0;function
s(b,a){return[0,[0,a,M(w(b+1|0,0))],0]}function
n(o,n){var
e=o,d=n,b=0;for(;;){if(e){if(d){var
h=d[2],j=d[1],k=e[2],f=e[1],l=T(f[2],j[2]);if(0<l){var
e=k,b=[0,f,b];continue}if(0<=l){var
m=a(i[15],f[1],j[1]);if(a(i[9],m,al)){var
e=k,d=h;continue}var
e=k,d=h,b=[0,[0,m,f[2]],b];continue}var
d=h,b=[0,j,b];continue}return a(g[8],b,e)}return d?a(g[8],b,d):c(g[6],b)}}function
t(n,e,b){function
c(g){var
o=g[2],p=g[1],c=j(e),d=w(c+1|0,0),h=0;if(!(c<0)){var
b=h;for(;;){var
k=f(o,b)[b+1],l=f(e,b)[b+1]+k|0;f(d,b)[b+1]=l;var
m=b+1|0;if(c!==b){var
b=m;continue}break}}return[0,a(i[22],n,p),d]}return a(cL[12],c,b)}function
u(a){return c(i[1],[0,a])}function
aw(c,a){var
b=w(c+1|0,0);f(b,a)[a+1]=1;var
d=M(b);return[0,[0,u(1),d],0]}function
ax(a){function
b(a){if(a){var
d=a[1],e=d[2],f=d[1],g=b(a[2]);return[0,[0,c(i[24],f),e],g]}return 0}return b(a)}function
v(e,b){function
c(b){if(b){var
d=b[1],f=d[2],g=d[1],h=c(b[2]);return[0,[0,a(i[22],e,g),f],h]}return 0}return c(b)}function
F(e,d){var
a=e,b=0;for(;;){if(a){var
c=a[1],f=a[2],a=f,b=n(t(c[1],c[2],d),b);continue}return b}}function
ay(a,b){if(a){if(0===b)return[0,[0,m,w(j(c(g[3],a)[2])+1|0,0)],0];var
d=function(b,a){if(1===a)return b;var
c=d(b,a/2|0),e=F(c,c);return 0===(a%2|0)?e:F(b,e)};return d(a,b)}return 0}function
X(c){if(c){var
d=c[2],b=c[1][1];if(d){if(!a(i[9],b,m))if(!a(i[9],b,am)){var
e=X(d);return a(i[48],b,e)}return b}return b}return m}function
Y(c,b){return a(i[48],c,b)}var
y=[0,[0,0],0,0];function
b(a){return a[1][1]}function
o(a){var
d=b(a);return c(g[3],d)[2]}var
p=[0,0],k=[0,w(aH,y)];function
Z(e,d){p[1]=p[1]+1|0;if(k[1].length-1<=p[1])k[1]=a(aq[5],k[1],w(p[1],y));var
b=[0,[0,e],p[1],d],c=p[1];f(k[1],c)[c+1]=b;return b}function
_(f,e){var
a=e;for(;;){if(a){var
d=a[1],h=a[2],i=b(d);if(0===V(f,c(g[3],i)[2])){var
a=h;continue}return d}return y}}function
az(e,d,c,b,a){var
f=t(b,a,d);return n(v(c,e),f)}var
$=a(C[1],0,aH);function
aa(a){throw U}function
A(c,b){return a(i[45],c,b)}function
ab(e,s){function
g(d){if(d){var
h=d[1],e=h[2],j=h[1],t=d[2];try{var
C=aa(e),l=C}catch(a){a=z(a);if(a!==U)throw a;var
k=_(e,s);b(k);var
l=k}var
f=b(l);if(f){var
n=f[1],o=n[1],u=f[2],v=n[2],p=Y(j,o),q=A(o,p),w=A(j,p),x=c(i[24],w),r=g(az(t,u,q,x,B(e,v))),y=r[2];return[0,a(i[22],q,r[1]),y]}return[0,m,d]}return[0,m,0]}var
d=g(e);return[0,d[1],d[2]]}var
q=[0,0],G=[0,0],H=a(C[1],0,51);function
ac(c,b){try{var
d=a(C[6],H,[0,c[2],b[2]]);return d}catch(a){a=z(a);if(a===U)return 0;throw a}}function
I(c,b,a){return h(C[5],H,[0,c[2],b[2]],a)}function
aA(d,c){q[1]=c;function
e(a){return X(b(a))}G[1]=a(g[13],e,c);function
f(a){return I(a,a,s(d,u(1)))}return a(g[11],f,c)}function
ae(f,p,e){function
k(a){if(a){var
g=a[1],d=g[2],q=a[2],r=g[1];try{var
w=aa(d),e=w}catch(a){a=z(a);if(a!==U)throw a;var
h=_(d,p);b(h);var
e=h}var
f=b(e);if(f){var
j=f[1],s=f[2],u=j[2],v=A(r,j[1]),l=c(i[24],v),m=B(d,u),o=k(n(q,t(l,m,s)));return[0,[0,[0,l,m,e],o[1]],o[2]]}return[0,0,a]}return cM}var
d=k(f),l=d[2],m=d[1],o=q[1];function
r(c,f){function
d(d,c){var
e=c[2],g=c[3],h=c[1],i=b(f);if(a(N,b(g),i)){var
k=u(1);return n(d,t(h,e,s(j(e),k)))}return d}return h(g[16],d,c,m)}return[0,h(g[19],r,e,o),l]}var
Q=[0,0],J=[0,y],aB=[0,1];function
R(b){var
c=0;function
e(c,b){var
e=f(b[2],0)[1];return a(d[5],c,e)}return Z(b,h(g[16],e,c,b))}function
aC(k,h){var
l=b(k),m=b(h),e=c(g[3],l)[2],q=c(g[3],m)[2],r=c(g[3],l)[1],v=c(g[3],m)[1],C=c(g[4],l),E=c(g[4],m),w=Y(r,v),x=D(e,q),y=B(x,e),z=B(x,q);function
o(b,a){var
d=A(r,w),e=t(c(i[24],d),z,a);return n(t(A(v,w),y,b),e)}var
F=o(C,E),G=h[3],H=f(z,0)[1]+G|0,J=k[3],K=f(y,0)[1]+J|0,p=Z(F,a(d[5],K,H)),L=u(1);I(p,k,o(s(j(e),L),0));var
M=u(1);I(p,h,o(0,s(j(e),M)));return p}function
K(c,b){return a(d[22],b,[0,c,0])}function
af(b,a){return T(b[2],a[2])}function
ag(b,a){return h(g[45],af,b,a)}function
ah(l,e){var
i=0;function
k(E,m){var
x=b(m),y=b(l),n=c(g[3],y)[2],v=c(g[3],x)[2],h=[0,1],e=[0,1],w=j(n);for(;;){if(h[1])if(e[1]<=w){var
p=e[1],q=0===f(n,p)[p+1]?1:0;if(q)var
s=q;else
var
t=e[1],s=0===f(v,t)[t+1]?1:0;h[1]=s;e[1]=e[1]+1|0;continue}if(h[1])var
u=0;else
var
A=o(m),B=D(o(l),A),i=m[2],k=l[2],z=0,C=r.caml_lessthan(k,i)?[0,k,i]:[0,i,k],u=[0,[0,C,B],z];return a(d[22],E,u)}}var
m=h(g[16],k,i,e);return a(g[41],af,m)}function
aD(a){function
c(a){if(a){var
b=a[2],d=a[1];if(b){var
e=c(b);return ag(ah(d,b),e)}}return 0}return c(a)}function
aF(b,i,l){var
e=b[2],h=b[1],c=h[2],d=h[1];function
j(a){var
h=a[2]!==d?1:0;if(h){var
i=a[2]!==c?1:0;if(i){var
j=V(e,o(a));if(j){var
l=a[2]<c?1:0;if(l)var
g=l;else
var
p=o(a),g=1-ao(e,D(o(f(k[1],d)[d+1]),p));if(g){var
m=a[2]<d?1:0;if(m)var
b=m;else
var
n=o(a),b=1-ao(e,D(o(f(k[1],c)[c+1]),n))}else
var
b=g}else
var
b=j}else
var
b=i}else
var
b=h;return b}return a(g[24],j,i)}var
ai=[0,0];function
aG(c,b,a){return ag(ah(c,b),a)}var
L=[0,m];function
aI(f,e){var
b=0===ai[1]?1:0;if(b){var
h=c(g[1],e),i=c(d[20],h),j=a(d[16],i,cN),k=a(d[16],cO,j),m=c(g[1],f),n=c(d[20],m),o=a(d[16],n,k),p=a(d[16],cP,o);return c(l[5],p)}return b}function
S(j,f,p){var
r=ab(b(J[1]),f),k=r[2],z=r[1],A=E(k),B=a(d[16],A,cQ),C=a(d[16],cR,B);c(l[5],C);L[1]=a(i[22],L[1],z);J[1]=R(k);if(0===k){c(l[5],cS);var
D=q[1],G=function(a){return 0},H=a(g[13],G,D),e=L[1],t=ae(v(e,j),f,H),w=t[1],I=t[2];c(l[5],cT);var
K=x(I),M=a(d[16],K,cU),N=a(d[16],cV,M);c(l[5],N);var
m=[0,v(e,j)],O=q[1],P=function(c,a){var
d=F(c,b(a));m[1]=n(m[1],d);return 0};h(g[18],P,w,O);var
Q=x(m[1]),S=a(d[16],Q,cW),T=a(d[16],cX,S);c(l[5],T);var
U=x(s(1,e)),V=a(d[16],U,cY),W=a(d[16],cZ,V);c(l[5],W);var
y=function(b){if(b){var
c=b[2],d=b[1],e=y(c),f=function(a){return ac(d,a)};return[0,a(g[13],f,c),e]}return 0},X=c(g[6],p),Y=y(c(g[6],f)),o=[0,c(g[6],Y)],Z=function(a){o[1]=c(g[4],o[1]);return 0};a(g[11],Z,p);var
_=o[1],$=c(g[6],w),aa=function(a){return v(u(-1),a)};return[0,X,j,[0,e,1,_,a(g[13],aa,$)]]}throw aj}var
aJ=[0,0];function
ak(a){return a?f(a[1][2],0)[1]:-1}function
aK(e,t,s){c(l[5],c0);ai[1]=0;c(C[2],$);return function(B){var
j=B;for(;;){var
o=j[2],i=j[1];aI(i,o);if(o){var
n=o[2],u=o[1],w=u[1],p=w[2],r=w[1];if(aF([0,[0,r,p],u[2]],i,n)){c(l[5],c1);var
j=[0,i,n];continue}var
D=f(k[1],p)[p+1],e=aC(f(k[1],r)[r+1],D);if(Q[1])if(0!==b(e)){var
Z=ak(b(J[1]));if(Z<ak(b(e))){c(l[5],c5);var
j=[0,i,n];continue}}var
x=ab(b(e),i),y=x[1];if(x[2]){var
F=v(y,b(e));e[1][1]=F;var
L=q[1],M=function(b,c){return function(a){return v(c,ac(b,a))}}(e,y),N=a(g[13],M,L),A=ae(b(e),i,N),O=A[1];e[1][1]=A[2];var
P=q[1],R=function(c){return function(d,b){a(C[9],H,[0,c[2],b[2]]);return I(c,b,d)}}(e);h(g[18],R,O,P);var
T=E(b(e)),U=a(d[16],T,c2),V=a(d[16],c3,U);c(l[5],V);q[1]=K(e,i);G[1]=K(m,G[1]);try{var
X=K(e,i),Y=S(b(t),X,s);return Y}catch(a){a=z(a);if(a===aj){var
W=aG(e,i,n),j=[0,K(e,i),W];continue}throw a}}c(l[5],c4);var
j=[0,i,n];continue}return S(b(t),i,s)}}(e)}function
aM(b){if(b){var
c=b[2],d=f(b[1][2],0)[1],e=function(a){return ao(f(a[2],0)[1],d)};return a(g[23],e,c)}return 1}return[0,an,s,av,aw,N,O,n,ax,F,ay,G,function(o,j,i){c(C[2],$);c(C[2],H);p[1]=0;k[1]=w(aH,y);Q[1]=a(g[23],aM,[0,i,j]);if(Q[1])c(l[5],c6);var
q=E(i),r=a(d[16],q,c7),s=a(d[16],c8,r);c(l[5],s);function
t(c,b){var
e=E(b),f=a(d[16],e,c_);return a(d[16],c,f)}var
u=h(g[16],t,c9,j),v=a(d[16],c$,u);c(l[5],v);var
e=a(g[13],R,j),n=R(i);aA(o,e);L[1]=m;J[1]=n;try{var
D=S(b(n),e,e),f=D}catch(a){a=z(a);if(a!==aj)throw a;var
f=aK([0,e,aD(e)],n,e)}var
x=f[3],A=f[2],B=f[1];c(l[5],da);return[0,a(g[13],b,B),A,x]},au,aB,aJ]},aj,aL];R(bb,A,"Nsatz_plugin.Ideal");var
aM=y[35],db=c(aM,0);c(aM,1);var
aN=m[49],aO=y[24],aP=y[27],aQ=y[16],dc=m[48],dd=y[25],de=y[4],df=y[5],dg=y[10],dh=y[8],di=y[3],dj=y[15],dk=y[33];function
dl(a){try{var
b=c(y[37],a);return b}catch(a){a=z(a);if(a[1]===ar)return 1;throw a}}var
dm=y[19];function
dn(e,d){var
c=e,b=d;for(;;){if(a(aO,b,db))return c;if(a(aP,c,b)){var
g=b,b=c,c=g;continue}var
f=a(aQ,c,b),c=b,b=f;continue}}function
aR(b){return a(m[32],b,D)?0:[0,b]}function
aS(a){var
b=a[2],c=a[1];return 1===b?c:[6,c,b]}function
av(a){var
b=a[1];if(typeof
b==="number")return a[2];var
c=a[2];return typeof
c==="number"?b:[3,b,c]}function
aT(c){var
b=c[1];if(typeof
b==="number")return 0;var
d=c[2];if(typeof
d==="number")return 0;else
if(0===d[0])if(a(m[32],d[1],N))return b;if(typeof
b!=="number"&&0===b[0]){var
e=c[2];if(a(m[32],b[1],N))return e}return[5,b,c[2]]}c(v[108],1);var
aw=[j,function(a){return h(t[4],dr,dq,dp)}],O=[j,function(a){return h(t[4],du,dt,ds)}],W=[j,function(a){return h(t[4],dx,dw,dv)}],X=[j,function(a){return h(t[4],dA,dz,dy)}],Y=[j,function(a){return h(t[4],dD,dC,dB)}],Z=[j,function(a){return h(t[4],dG,dF,dE)}],_=[j,function(a){return h(t[4],dJ,dI,dH)}],$=[j,function(a){return h(t[4],dM,dL,dK)}],aU=[j,function(a){return h(t[4],dO,ax,dN)}],aV=[j,function(a){return h(t[4],dQ,ax,dP)}],aW=[j,function(a){return h(t[4],dS,ax,dR)}],i=[j,function(a){return h(t[4],dU,E,dT)}],ak=[j,function(a){return h(t[4],dW,E,dV)}],aa=[j,function(a){return h(t[4],dY,E,dX)}],d1=[j,function(a){return h(t[4],d0,E,dZ)}],d4=[j,function(a){return h(t[4],d3,E,d2)}],ab=[j,function(a){return h(t[4],d6,E,d5)}],al=[j,function(a){return h(t[4],d8,E,d7)}],am=[j,function(a){return h(t[4],d_,E,d9)}],eb=[j,function(a){return h(t[4],ea,E,d$)}];function
s(a,d){var
b=p(a),f=c(e[19][12],d),g=q===b?a[1]:j===b?c(o[2],a):a;return c(v[bk],[0,g,f])}function
ay(f){var
a=p(i),b=0,d=0,e=q===a?i[1]:j===a?c(o[2],i):i;return s(aU,[0,s(aw,[0,e,d]),b])}function
P(b){if(a(m[26],b,N)){var
d=p(al);return q===d?al[1]:j===d?c(o[2],al):al}var
e=a(m[12],b,V);return a(m[26],e,D)?s(ab,[0,P(a(m[11],b,V)),0]):s(d4,[0,P(a(m[11],b,V)),0])}function
aX(b){if(a(m[26],b,D)){var
d=p(ak);return q===d?ak[1]:j===d?c(o[2],ak):ak}return a(m[28],b,D)?s(aa,[0,P(b),0]):s(d1,[0,P(a(m[4],ec,b)),0])}function
F(A){var
b=A;for(;;)if(typeof
b==="number"){var
b=[0,D];continue}else
switch(b[0]){case
0:var
x=c(m[50],b[1]),d=c(au[5],x),y=c(au[3],d);c(m[48],y);var
z=c(au[2],d),f=p(i),B=[0,aX(c(m[48],z)),0],C=q===f?i[1]:j===f?c(o[2],i):i;return s(O,[0,C,B]);case
1:var
g=p(i),E=[0,P(c(m[43],b[1])),0],G=q===g?i[1]:j===g?c(o[2],i):i;return s(W,[0,G,E]);case
2:var
h=p(i),H=[0,F(b[1]),0],I=q===h?i[1]:j===h?c(o[2],i):i;return s(_,[0,I,H]);case
3:var
J=b[1],K=[0,F(b[2]),0],k=p(i),L=[0,F(J),K],M=q===k?i[1]:j===k?c(o[2],i):i;return s(X,[0,M,L]);case
4:var
Q=b[1],R=[0,F(b[2]),0],l=p(i),S=[0,F(Q),R],T=q===l?i[1]:j===l?c(o[2],i):i;return s(Y,[0,T,S]);case
5:var
U=b[1],V=[0,F(b[2]),0],n=p(i),aa=[0,F(U),V],ab=q===n?i[1]:j===n?c(o[2],i):i;return s(Z,[0,ab,aa]);default:var
r=b[2],ac=b[1];if(0===r){var
t=p(i),ad=[0,aX(N),0],ae=q===t?i[1]:j===t?c(o[2],i):i;return s(O,[0,ae,ad])}var
u=c(m[45],r),af=0;if(a(m[32],u,D))var
e=p(am),v=q===e?am[1]:j===e?c(o[2],am):am;else
var
v=s(eb,[0,P(u),0]);var
w=p(i),ag=[0,F(ac),[0,v,af]],ah=q===w?i[1]:j===w?c(o[2],i):i;return s($,[0,ah,ag])}}function
Q(g){var
b=c(v[ag],g);if(9===b[0]){var
d=b[2];if(1===d.length-1){var
e=d[1],f=p(ab),h=b[1],i=q===f?ab[1]:j===f?c(o[2],ab):ab;if(a(v[H],h,i)){var
k=Q(e);return a(m[6],V,k)}var
l=Q(e),n=a(m[6],V,l);return a(m[1],N,n)}}return N}function
G(M){var
i=c(v[ag],M);if(9===i[0]){var
d=i[2],x=d.length-1,b=i[1];if(2===x){var
k=d[2],y=p(W),N=q===y?W[1]:j===y?c(o[2],W):W;if(a(v[H],b,N)){var
P=Q(k);return[1,c(m[40],P)]}var
z=p(O),R=q===z?O[1]:j===z?c(o[2],O):O;if(a(v[H],b,R)){var
h=c(v[ag],k);if(9===h[0]){var
r=h[2];if(1===r.length-1){var
s=r[1],t=p(aa),J=h[1],K=q===t?aa[1]:j===t?c(o[2],aa):aa;if(a(v[H],J,K))var
l=Q(s),g=1;else
var
L=Q(s),l=a(m[4],D,L),g=1}else
var
g=0}else
var
g=0;if(!g)var
l=D;return[0,l]}var
A=p(_),S=q===A?_[1]:j===A?c(o[2],_):_;return a(v[H],b,S)?[2,G(k)]:0}if(3===x){var
e=d[2],f=d[3],B=p(X),T=q===B?X[1]:j===B?c(o[2],X):X;if(a(v[H],b,T)){var
U=G(f);return[3,G(e),U]}var
C=p(Y),V=q===C?Y[1]:j===C?c(o[2],Y):Y;if(a(v[H],b,V)){var
ab=G(f);return[4,G(e),ab]}var
E=p(Z),ac=q===E?Z[1]:j===E?c(o[2],Z):Z;if(a(v[H],b,ac)){var
ad=G(f);return[5,G(e),ad]}var
F=p($),ae=q===F?$[1]:j===F?c(o[2],$):$;if(a(v[H],b,ae)){var
u=c(v[ag],f);if(9===u[0]){var
w=u[2];if(1===w.length-1)var
I=Q(w[1]),n=1;else
var
n=0}else
var
n=0;if(!n)var
I=D;var
af=c(m[44],I);return[6,G(e),af]}return 0}}return 0}function
aY(e){var
b=c(v[ag],e);if(9===b[0]){var
a=b[2],d=a.length-1-1|0;if(!(2<d>>>0))switch(d){case
0:return 0;case
1:break;default:var
f=a[2],g=aY(a[3]);return[0,G(f),g]}}throw[0,J,ed]}var
M=[0,0];function
ee(b){function
c(e){var
b=e;for(;;)if(typeof
b==="number")return 0;else
switch(b[0]){case
0:return 0;case
1:var
f=a5(b[1]);M[1]=a(d[5],M[1],f);return 0;case
2:var
b=b[1];continue;case
3:var
g=b[2];c(b[1]);var
b=g;continue;case
4:var
h=b[2];c(b[1]);var
b=h;continue;case
5:var
i=b[2];c(b[1]);var
b=i;continue;default:var
b=b[1];continue}}return c(b)}var
aZ=c(as[1],[0,aO,aP,dd,de,df,dg,dh,di,dj,aQ,dm,dn,dl,aN,dk]),k=c(A[1],aZ);function
az(j,b){var
g=c(k[1],b);function
i(g){if(g){var
p=g[2],q=g[1],r=q[2].length-1-1|0,t=q[1],u=function(g,k){var
c=k[2],e=g[2],d=g[1],a=[0,0],l=1;if(!(r<1)){var
b=l;for(;;){if(0<f(c,b)[b+1])a[1]=b;var
o=b+1|0;if(r!==b){var
b=o;continue}break}}if(0===a[1])return[0,d,e];if(d<a[1]){var
h=a[1],m=f(c,h)[h+1];return[0,a[1],m]}if(a[1]===d){var
i=a[1];if(f(c,i)[i+1]<e){var
j=a[1],n=f(c,j)[j+1];return[0,a[1],n]}}return[0,d,e]},s=h(e[17][15],u,ef,g),j=s[2],b=s[1];if(0===b){var
n=function(b){if(0===b[0])return aR(c(dc,b[1]));var
f=[0,0],g=b[2],h=b[1];function
i(b,a){var
e=aS([0,[1,c(d[20],h)],b]),g=aT([0,n(a),e]);f[1]=av([0,f[1],g]);return 0}a(e[19][14],i,g);return f[1]},o=n(t);return c(e[17][47],p)?o:av([0,o,i(p)])}var
k=[0,0],l=[0,0],v=function(c){var
a=c[2],d=c[1];return j<=f(a,b)[b+1]?(a[b+1]=f(a,b)[b+1]-j|0,k[1]=[0,[0,d,a],k[1]],0):(l[1]=[0,[0,d,a],l[1]],0)};a(e[17][11],v,g);var
w=1===j?[1,c(d[20],b)]:aS([0,[1,c(d[20],b)],j]),x=i(c(e[17][6],l[1]));return av([0,aT([0,w,i(c(e[17][6],k[1]))]),x])}return aR(c(m[43],eg))}return i(g)}function
a1(b,a){function
d(b,a){if(b){if(0===b[1]){var
c=b[2];if(a){var
e=a[1];return[0,e,d(c,a[2])]}throw[0,J,ep]}var
f=d(b[2],a);return[0,k[3],f]}return a}var
f=d(b,c(e[17][6],a));return c(e[17][6],f)}function
eJ(a3){var
B=aY(a3);M[1]=0;a(e[17][11],ee,B);if(B){var
G=B[1];if(typeof
G==="number")var
t=0;else
if(0===G[0]){var
S=G[1];if(0===S[0]){var
H=B[2];if(H){var
I=H[1];if(typeof
I==="number")var
L=1;else
if(0===I[0]){var
V=I[1],W=S[1];if(0===V[0]){var
X=V[1],at=H[2];if(7<W>>>0)c(T[7],er);else
switch(W){case
0:c(l[5],eB);A[3][1]=0;k[14][1]=0;k[15][1]=0;break;case
1:c(l[5],eC);A[3][1]=0;k[14][1]=1;k[15][1]=0;break;case
2:c(l[5],eD);A[3][1]=1;k[14][1]=0;k[15][1]=0;break;case
3:c(l[5],eE);A[3][1]=1;k[14][1]=1;k[15][1]=0;break;case
4:c(l[5],eF);A[3][1]=0;k[14][1]=0;k[15][1]=1;break;case
5:c(l[5],eG);A[3][1]=0;k[14][1]=1;k[15][1]=1;break;case
6:c(l[5],eH);A[3][1]=1;k[14][1]=0;k[15][1]=1;break;default:c(l[5],eI);A[3][1]=1;k[14][1]=1;k[15][1]=1}var
Y=M[1],r=[0,0];if(!(Y<1)){var
y=Y;for(;;){var
aS=r[1],aT=c(d[20],y),aU=a(d[16],aT,ez),aX=[0,a(d[16],eA,aU),0];r[1]=a(e[18],aX,aS);var
a2=y-1|0;if(1!==y){var
y=a2;continue}break}}r[1]=a(e[18],es,r[1]);k[6][1]=r[1];var
au=function(b){var
f=M[1];function
d(b){if(typeof
b==="number")var
e=k[3];else
switch(b[0]){case
0:var
h=b[1];if(a(m[32],h,D))var
i=k[3];else
var
l=[0,c(aN,h)],i=a(k[2],f,l);var
e=i;break;case
1:var
g=a5(b[1]);if(g<=X)var
n=c(aZ[2],g),j=a(k[2],f,n);else
var
j=a(k[4],f,g);var
e=j;break;case
2:var
o=d(b[1]),e=c(k[8],o);break;case
3:var
p=b[1],q=d(b[2]),r=d(p),e=a(k[7],r,q);break;case
4:var
s=b[1],t=d(b[2]),u=c(k[8],t),v=d(s),e=a(k[7],v,u);break;case
5:var
w=b[1],x=d(b[2]),y=d(w),e=a(k[9],y,x);break;default:var
z=b[2],A=d(b[1]),e=a(k[10],A,z)}return e}return d(b)},K=a(e[17][12],au,at);if(K){var
av=K[1],ax=c(e[17][6],K[2]),P=c(k[13][1],12),as=function(b){try{var
c=a(k[13][7],P,b);return c}catch(a){a=z(a);if(a===U){h(k[13][5],P,b,1);return 0}throw a}},Q=function(b){if(b){var
c=b[1],d=Q(b[2]),e=d[2],f=d[1];if(!a(k[5],c,k[3]))if(!as(c))return[0,[0,c,f],[0,0,e]];return[0,f,[0,1,e]]}return eo},R=Q(ax),Z=R[2],aA=R[1],ao=c(a0[87],0),u=h(k[12],M[1],aA,av)[3];c(e[17][6],k[11][1]);var
ap=c(a0[87],0)-ao,aq=a(ai[99],em,ap),ar=a(d[16],en,aq);c(l[5],ar);c(l[5],et);var
aB=c(e[17][6],u[3]),v=[0,u[4],aB],_=c(e[17][3],v),$=c(e[17][1],_),b=c(e[17][1],v),C=w(b,0),N=function(d){if(b<=d)return 0;f(C,d)[d+1]=1;var
h=a(e[17][5],v,d),i=c(e[17][1],h)-1|0,j=0;if(!(i<0)){var
g=j;for(;;){var
l=a(e[17][5],h,g);if(1-a(k[5],l,k[3]))N((g+d|0)+1|0);var
m=g+1|0;if(i!==g){var
g=m;continue}break}}return 0};N(0);var
E=[0,0],O=b-1|0,aa=0;if(!(O<0)){var
g=aa;for(;;){if(f(C,g)[g+1]){var
am=E[1];E[1]=[0,a(e[17][5],v,g),am]}var
an=g+1|0;if(O!==g){var
g=an;continue}break}}var
ab=c(e[17][6],E[1]),ac=function(j){var
d=[0,j],h=b-1|0,k=0;if(!(h<0)){var
a=k;for(;;){if(1-f(C,a)[a+1]){var
l=(b-a|0)+($-b|0)|0,m=d[1],g=function(a,b){if(c(e[17][47],a))return 0;if(0<=b){if(0===b)return c(e[17][4],a);if(a){var
d=a[1];return[0,d,g(a[2],b-1|0)]}throw[0,J,eh]}return a},i=g(c(e[17][6],m),l);d[1]=c(e[17][6],i)}var
n=a+1|0;if(h!==a){var
a=n;continue}break}}return d[1]},n=a(e[17][12],ac,ab),ad=b-c(e[17][1],n)|0,ae=c(d[20],ad),af=a(d[16],ae,ei),ag=a(d[16],ej,af);c(l[5],ag);var
ah=c(e[17][1],n),aj=c(d[20],ah),ak=a(d[16],aj,ek),al=a(d[16],el,ak);c(l[5],al);if(n){var
aC=n[2],aD=a1(Z,n[1]),x=M[1],aE=az(x,a(k[2],x,u[1])),aF=[6,0,u[2]],aG=c(e[17][6],aC),aH=function(a){return a1(Z,a)},aI=a(e[17][12],aH,aG),aJ=function(a){return az(x,a)},aK=c(e[17][12],aJ),aL=a(e[17][12],aK,aI),aM=function(a){return az(x,a)},aO=a(e[17][12],aM,aD),aP=c(d[20],X),aQ=a(d[16],aP,eu),aR=a(d[16],ev,aQ);c(l[5],aR);c(l[5],ew);var
a4=a(e[18],[0,[0,aE,[0,aF,aO]],0],aL),a6=function(b){return a(e[17][12],F,b)},a7=a(e[17][12],a6,a4),a8=s(aV,[0,ay(0),0]),a9=function(d,b){var
a=p(i),f=0,g=0,k=q===a?i[1]:j===a?c(o[2],i):i,l=s(aV,[0,s(aw,[0,k,g]),f]);function
m(d,b){var
a=p(i),e=[0,d,[0,b,0]],f=0,g=q===a?i[1]:j===a?c(o[2],i):i;return s(aW,[0,s(aw,[0,g,f]),e])}var
n=[0,h(e[17][16],m,d,l),[0,b,0]];return s(aW,[0,ay(0),n])},a_=h(e[17][16],a9,a7,a8);c(l[5],eK);return a_}throw[0,J,ex]}throw[0,J,ey]}var
t=1,L=0}else
var
L=1;if(L)var
t=1}else
var
t=1}else
var
t=1}else
var
t=0}throw[0,J,eq]}function
eL(a){var
b=[0,s(aU,[0,ay(0),0]),a],d=[0,h(t[4],eO,eN,eM),b],e=[0,c(v[bk],d),0];return c(eP[148],e)}var
aA=[0,function(a){try{var
d=eJ(a),b=d}catch(a){a=z(a);if(a!==A[2])throw a;var
b=c(T[7],eQ)}return eL(b)}];R(267,aA,"Nsatz_plugin.Nsatz");c(a2[12],eR);function
eS(e){var
a=[31,eU[4],[0,[0,an,eT],0],0],b=[28,[0,[0,[0,c(aB[1][7],eV)],0],a]],d=c(aB[1][6],eW);return e4(aC[6][4],1,0,d,b)}function
eX(a){if(a)if(!a[2]){var
b=a[1];return function(a){return c(aA[1],b)}}return c(d[2],eY)}var
e0=[0,[0,c(aB[1][7],eZ)],0],e1=[0,a(aC[12][27],e0,eX)];h(aC[6][9],0,[0,an,e2],e1);a(a2[19],eS,an);var
a3=[0,an];R(272,a3,"Nsatz_plugin.G_nsatz");R(273,[0,l,as,A,aA,a3],"Nsatz_plugin");return});
