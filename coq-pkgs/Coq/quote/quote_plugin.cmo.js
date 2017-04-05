(function(bN){"use strict";var
p="plugins/quote/g_quote.ml4",aA="in",C=250,aw="quote_plugin",av="$k",au="[",n=246,az="$c",at=132,S="Extension: cannot occur",q=120,s="quote",ay="]",as="$lc",r="Quote",R="$f",ar="plugins/quote/quote.ml",B=136,ax="using",m=bN.jsoo_runtime,aq=m.caml_check_bound,b=m.caml_new_string,A=m.caml_obj_tag,ab=m.caml_register_global,$=m.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):m.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):m.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):m.caml_call_gen(a,[b,c,d])}function
aa(a,b,c,d,e){return a.length==4?a(b,c,d,e):m.caml_call_gen(a,[b,c,d,e])}var
e=m.caml_get_global_data(),Q=b(aw),d=e.Term,f=e.Util,x=e.Tacmach,k=e.Assert_failure,N=e.Proofview,an=e.Tactics,U=e.Evd,M=e.Global,am=e.Constr_matching,h=e.Names,v=e.Termops,ad=e.CErrors,ac=e.Coqlib,y=e.CamlinternalLazy,ai=e.Option,ah=e.Patternops,w=e.Pervasives,g=e.Ltac_plugin,o=e.Loc,i=e.Stdarg,l=e.Genarg,ao=e.Mltop,a0=[0,b(ar),473,11],aZ=[0,b(ar),458,13],aW=b("invalid inversion scheme for quote"),aU=[0,b("Coq"),[0,b(s),[0,b(r),0]]],aQ=b("M"),aP=b("Quote: not a simple fixpoint"),aN=b("End_idx"),aO=[0,b(r),0],aL=b("Left_idx"),aM=[0,b(r),0],aJ=b("Right_idx"),aK=[0,b(r),0],aH=b("varmap_find"),aI=[0,b(r),0],aF=b("Node_vm"),aG=[0,b(r),0],aD=b("Empty_vm"),aE=[0,b(r),0],aB=b(s),aC=b(r),bK=[0,b(p),1,0],bH=[0,b(p),1,0],bE=[0,b(p),1,0],bA=[0,b(p),1,0],bx=[0,b(p),1,0],bu=[0,b(p),1,0],br=[0,b(p),1,0],bo=[0,b(p),1,0],bk=[0,b(p),1,0],bh=[0,b(p),1,0],bg=b(av),bi=[0,b(ax)],bj=b(az),bl=[0,b(aA)],bm=[0,b(ay)],bn=b(as),bp=[0,b(au)],bq=b(R),bs=[0,b(s)],bt=b(av),bv=[0,b(ax)],bw=b(az),by=[0,b(aA)],bz=b(R),bB=[0,b(s)],bC=[0,[0,b(ay)],0],bD=b(as),bF=[0,b(au)],bG=b(R),bI=[0,b(s)],bJ=b(R),bL=[0,b(s)],bM=b(s),ba=b(S),a_=b(S),a8=b(S),a6=b(S),a1=b(aw),a2=b("cont"),a3=b("x"),be=b(s),aV=e.Not_found,aX=e.Pp,aR=e.Environ,aS=e.Reductionops,aT=e.Hashtbl,a4=e.Geninterp,bc=e.Array;function
t(b,a){return j(ac[4],aC,[0,aB,b],a)}var
D=[n,function(a){return t(aE,aD)}],E=[n,function(a){return t(aG,aF)}],F=[n,function(a){return t(aI,aH)}],G=[n,function(a){return t(aK,aJ)}],H=[n,function(a){return t(aM,aL)}],I=[n,function(a){return t(aO,aN)}],K=a(f[20][1],[0,d[137]]);function
u(b){return a(ad[7],aP)}function
T(b){var
c=a(v[67],b);return a(d[B],c)}function
ae(c){var
b=a(h[1][8],c);return m.caml_int_of_string(j(f[15][4],b,1,m.caml_ml_string_length(b)-1|0))}function
af(b){var
d=a(w[20],b),e=c(w[16],aQ,d);return a(h[1][6],e)}function
L(j,i,b){var
e=a(d[B],j);if(11===e[0]){var
g=e[1],h=g[1];if(0===h[2]){var
k=g[2],l=h[1],m=function(c){return a(d[110],b-c|0)},n=c(f[19][2],b,m),o=[0,a(d[127],[0,[0,[0,l,0],i+1|0],k]),n];return a(d[q],o)}}return u(0)}function
ag(b,k){function
h(l){var
e=l;for(;;){var
b=a(d[B],e);switch(b[0]){case
5:var
e=b[1];continue;case
9:var
i=b[2],g=b[1];if(a(d[1],g))if(a(d[29],g)===k){var
o=a(f[19][38],i);return[11,[0,af(a(d[29],o))]]}var
p=c(f[19][15],h,i),q=U[16],r=a(M[2],0);return[4,j(ah[7],r,q,g),p];default:var
m=U[16],n=a(M[2],0);return j(ah[7],n,m,e)}}}return h(b)}function
V(E,D,l){try{var
Y=a(d[41],E),m=Y}catch(a){a=$(a);if(a!==d[28])throw a;var
m=u(0)}var
G=a(M[2],0),o=T(c(aR[58],G,m));if(14===o[0]){var
p=o[1],q=p[1];if(1===q[1].length-1)if(0===q[2]){var
e=p[2];if(1===e[1].length-1)if(1===e[2].length-1){var
r=e[3];if(1===r.length-1){var
s=a(d[80],r[1]),b=s[1],H=s[2],I=a(f[17][1],b),J=a(N[63][5],l),O=a(x[48][4],l),P=j(aS[77],0,J,O),g=T(H);if(13===g[0]){var
t=g[2],h=[0,0],i=[0,0],k=[0,0],Q=g[4],R=function(j,t){var
o=a(d[80],t),e=o[2],g=a(f[17][1],o[1]);if(a(d[1],e))if(1===a(d[29],e)){k[1]=[0,L(a(f[17][3],b)[2],j,g)];return 0}var
p=a(d[39],e),q=p[2],u=p[1];if(q){var
r=q[2];if(r){var
l=r[2];if(l){var
m=l[2];if(m)if(!m[2]){var
x=m[1];if(a(d[1],l[1]))if(a(d[1],x)){var
s=A(F),z=C===s?F[1]:n===s?a(y[2],F):F;if(c(P,u,z)){i[1]=[0,L(a(f[17][3],b)[2],j,g)];return 0}}}}}}var
v=h[1],w=ag(e,(I+g|0)+1|0);h[1]=[0,[0,L(a(f[17][3],b)[2],j,g),w],v];return 0};c(f[19][14],R,Q);var
w=a(ai[3],k[1]),S=w?a(ai[3],i[1]):w;if(S)u(0);var
z=a(d[B],t),U=7===z[0]?a(v[56],z[3]):t,V=k[1],W=j(f[17][16],K[4],D,K[1]),X=i[1];return[0,a(f[17][6],h[1]),X,U,W,V]}return u(0)}}}}return u(0)}function
O(e,j){var
g=j;for(;;){var
h=c(K[3],g,e);if(h)return h;var
b=a(d[B],g);switch(b[0]){case
5:var
g=b[1];continue;case
9:var
k=b[2],i=O(e,b[1]);if(i){var
l=function(a){return O(e,a)};return c(f[19][30],l,k)}return i;default:return 0}}}function
aj(e,c){var
h=e.length-1,b=A(E),l=h>>>1|0,i=C===b?E[1]:n===b?a(y[2],E):E,j=A(D),k=[0,c],m=C===j?D[1]:n===j?a(y[2],D):D,f=a(d[q],[0,m,k]);function
g(b){if(h<b)return f;if(l<b){var
j=b-1|0,m=[0,i,[0,c,aq(e,j)[j+1],f,f]];return a(d[q],m)}var
n=g((2*b|0)+1|0),k=b-1|0,o=g(2*b|0),p=[0,i,[0,c,aq(e,k)[k+1],o,n]];return a(d[q],p)}return g(1)}function
ak(e){function
b(a){return 1===a?0:[0,1===(a%2|0)?1:0,b(a>>>1|0)]}var
c=A(I),g=C===c?I[1]:n===c?a(y[2],I):I,h=b(e),i=a(f[17][6],h);function
k(g,f){var
h=[0,f];if(g)var
b=A(G),i=C===b?G[1]:n===b?a(y[2],G):G,c=i;else
var
e=A(H),j=C===e?H[1]:n===e?a(y[2],H):H,c=j;return a(d[q],[0,c,h])}return j(f[17][16],k,i,g)}function
W(h,k,g){var
b=k;for(;;){var
i=j(x[36],h,b,g);if(i)return i;var
e=a(d[B],b);switch(e[0]){case
5:var
b=e[1];continue;case
9:var
l=e[2],m=function(a){return W(h,a,g)};return c(f[19][28],m,l);default:return 0}}}function
al(f,a){function
g(a,b){if(b){var
h=b[2],e=b[1];return c(d[at],a,e)?b:W(f,a,e)?[0,a,[0,e,h]]:[0,e,g(a,h)]}return[0,a,0]}if(a){var
b=a[1];return g(b,al(f,a[2]))}return 0}var
P=a(aT[18],[0,d[at],d[147]]);function
X(d,b){a(ac[11],aU);var
i=a(P[1],17),g=[0,0],k=[0,1];function
l(b){function
e(r){var
e=r;for(;;){if(e){var
m=e[1],s=e[2],t=m[2],u=m[1];try{var
w=U[16],x=a(M[2],0),y=aa(am[3],x,w,t,b),z=a(h[1][11][17],y),A=function(a){var
b=a[1],c=l(a[2]);return[0,ae(b),c]},B=c(f[17][12],A,z),C=c(v[55],B,u);return C}catch(a){a=$(a);if(a===am[1]){var
e=s;continue}throw a}}var
n=d[2];if(n){var
o=d[5],D=n[1];if(o){var
E=o[1];if(O(d[4],b))return c(v[55],[0,[0,1,b],0],E)}try{var
G=c(P[7],i,b);return G}catch(a){a=$(a);if(a===aV){var
F=[0,[0,1,ak(k[1])],0],p=c(v[55],F,D);k[1]++;g[1]=[0,b,g[1]];j(P[5],i,b,p);return p}throw a}}var
q=d[5];if(q)return c(v[55],[0,[0,1,b],0],q[1]);var
H=a(aX[3],aW);return j(ad[3],0,0,H)}}return e(d[1])}var
e=c(f[17][12],l,b),m=d[3],n=a(f[17][6],g[1]);return[0,e,aj(a(f[19][12],n),m)]}function
aY(n,m){var
b=[0,function(b){var
e=c(x[48][2],n,b);function
o(a){return c(x[48][2],a,b)}var
h=V(e,c(f[17][12],o,m),b),i=X(h,[0,a(N[63][3],b),0]),g=i[1];if(g)if(!g[2]){var
l=g[1],p=i[2];if(h[2]){var
r=a(d[q],[0,e,[0,p,l]]);return j(an[3],0,r,2)}var
s=a(d[q],[0,e,[0,l]]);return j(an[3],0,s,2)}throw[0,k,aZ]}];return a(N[63][9],b)}var
z=[0,t,D,E,F,G,H,I,K,u,T,ae,af,L,ag,V,O,aj,ak,W,al,P,X,aY,function(h,o,n,m){var
b=[0,function(b){var
e=c(x[48][2],n,b);function
p(a){return c(x[48][2],a,b)}var
i=V(e,c(f[17][12],p,m),b),j=X(i,[0,o,0]),g=j[1];if(g)if(!g[2]){var
l=g[1],r=j[2];return i[2]?a(h,a(d[q],[0,e,[0,r,l]])):a(h,a(d[q],[0,e,[0,l]]))}throw[0,k,a0]}];return a(N[63][9],b)}];ab(84,z,"Quote_plugin.Quote");a(ao[12],a1);var
J=o[4],Y=a(h[1][6],a2),Z=a(h[1][6],a3);function
_(d,b){var
e=a(g[12][2][1],b),f=a4[4][1],i=c(h[1][11][5],Z,e),k=[0,j(h[1][11][4],Y,d,i),f];return c(g[12][18],k,[29,[0,J,[3,J,[1,[0,J,Y]],[0,[2,[1,[0,J,Z]]],0]]]])}var
a5=0,a7=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e){var
f=e[2];if(f)if(!f[2]){var
h=f[1],j=e[1],k=d[1],m=b[1],n=a(l[6],i[9]),o=c(g[12][2][7],n,m),p=a(l[17],i[9]),q=a(l[6],p),r=c(g[12][2][7],q,k),s=a(l[6],i[13]),t=c(g[12][2][7],s,j),u=a(l[6],g[1][1]),v=c(g[12][2][7],u,h);return function(b){function
a(a){return _(v,a)}return aa(z[24],a,t,o,r)}}}}}return a(w[2],a6)},a5],a9=[0,function(b){if(b){var
d=b[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],j=b[1],k=a(l[6],i[9]),m=c(g[12][2][7],k,j),n=a(l[6],i[13]),o=c(g[12][2][7],n,h),p=a(l[6],g[1][1]),q=c(g[12][2][7],p,f);return function(c){var
a=0;function
b(a){return _(q,a)}return aa(z[24],b,o,m,a)}}}}return a(w[2],a8)},a7],a$=[0,function(b){if(b){var
d=b[2];if(d)if(!d[2]){var
e=d[1],f=b[1],h=a(l[6],i[9]),j=c(g[12][2][7],h,f),k=a(l[17],i[9]),m=a(l[6],k),n=c(g[12][2][7],m,e);return function(a){return c(z[23],j,n)}}}return a(w[2],a_)},a9],bb=[0,function(b){if(b)if(!b[2]){var
d=b[1],e=a(l[6],i[9]),f=c(g[12][2][7],e,d);return function(a){return c(z[23],f,0)}}return a(w[2],ba)},a$],bd=a(bc[12],bb);j(g[6][9],0,[0,Q,be],bd);function
bf(O){var
t=a(h[1][7],bg),b=g[1][1],r=0,s=0;if(0===b[0]){var
u=[0,bi,[0,[1,o[4],[5,[0,b[1]]],t],s]],v=a(h[1][7],bj),c=i[13];if(0===c[0]){var
w=[0,bm,[0,bl,[0,[1,o[4],[5,[0,c[1]]],v],u]]],x=a(h[1][7],bn),d=i[9];if(0===d[0]){var
y=[0,bp,[0,[1,o[4],[0,[5,[0,d[1]]]],x],w]],z=a(h[1][7],bq),e=i[9];if(0===e[0]){var
A=[0,[0,bs,[0,[1,o[4],[5,[0,e[1]]],z],y]],r],C=a(h[1][7],bt),f=g[1][1],B=0;if(0===f[0]){var
D=[0,bv,[0,[1,o[4],[5,[0,f[1]]],C],B]],E=a(h[1][7],bw),l=i[13];if(0===l[0]){var
F=[0,by,[0,[1,o[4],[5,[0,l[1]]],E],D]],G=a(h[1][7],bz),m=i[9];if(0===m[0]){var
H=[0,[0,bB,[0,[1,o[4],[5,[0,m[1]]],G],F]],A],I=a(h[1][7],bD),n=i[9];if(0===n[0]){var
J=[0,bF,[0,[1,o[4],[0,[5,[0,n[1]]]],I],bC]],K=a(h[1][7],bG),p=i[9];if(0===p[0]){var
L=[0,[0,bI,[0,[1,o[4],[5,[0,p[1]]],K],J]],H],N=a(h[1][7],bJ),q=i[9],M=0;if(0===q[0])return j(g[9][4],[0,Q,bM],0,[0,[0,bL,[0,[1,o[4],[5,[0,q[1]]],N],M]],L]);throw[0,k,bK]}throw[0,k,bH]}throw[0,k,bE]}throw[0,k,bA]}throw[0,k,bx]}throw[0,k,bu]}throw[0,k,br]}throw[0,k,bo]}throw[0,k,bk]}throw[0,k,bh]}c(ao[19],bf,Q);var
ap=[0,Q,J,Y,Z,_];ab(92,ap,"Quote_plugin.G_quote");ab(93,[0,z,ap],"Quote_plugin");return});
