(function(fC){"use strict";var
b9=125,bk=131,aU=".",P=140,G=112,cg="cc",cl="$l",b8="  [",b5=3901498,b6=" : ",ck=-431191102,cf="$n",bj=-912009552,O=250,ce=105,W="Init",A=124,F=246,aY="congruence",cd="[",a0=115,an=130,aX="plugins/cc/g_congruence.ml4",aT=111,b4="cc_plugin",b7=151,aV="Extension: cannot occur",b3="A",aa=113,ah=122,cc="X",cj="with",aZ="]",cb=139,b2=915186972,bh=116,ca=888453194,V="Logic",b_="Congruence",b$=" and ",aW=109,b1=-318868643,ai=126,ci=121,ch="Heq",aS="f_equal",aR=15500,b0=129,bi=1e3,w=fC.jsoo_runtime,q=w.caml_check_bound,ax=w.caml_int_compare,aQ=w.caml_make_vect,d=w.caml_new_string,N=w.caml_obj_tag,ay=w.caml_register_global,r=w.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
l(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
B(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
fB(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
bg(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
h=w.caml_get_global_data(),ag=d(b4),e=h.Term,j=h.Names,aj=h.Hashset,bm=h.Sorts,i=h.Util,s=h.Not_found,aB=h.Global,aq=h.Environ,k=h.Tacmach,g=h.Int,t=h.Queue,c=h.Pp,a9=h.Control,C=h.CErrors,I=h.Pervasives,bz=h.Vars,by=h.Goal,ar=h.Assert_failure,S=h.Termops,a2=h.Feedback,ao=h.Hashtbl,K=h.CamlinternalLazy,au=h.Globnames,m=h.Proofview,p=h.Tacticals,E=h.Tactics,at=h.Typing,bS=h.Evarsolve,aM=h.Refiner,bc=h.Coqlib,bV=h.Universes,aK=h.Evd,_=h.CClosure,aw=h.Loc,bf=h.Tacenv,aP=h.Constrarg,aN=h.Stdarg,am=h.Genarg,aO=h.Tacinterp,be=h.Mltop,a1=[0,0],dR=d("Out of depth ... "),dQ=d("Out of instances ... "),dS=d("First run was incomplete, completing ... "),dP=d("Executing ... "),dO=d("Running E-matching algorithm ... "),dN=d("paf_of_patt: pattern is trivial"),dL=d("wrong incomplete class"),dF=d(" ... "),dG=d(" = "),dH=d("Checking if "),dE=d("Yes"),dI=d("No"),dB=d(aU),dC=d("Processing mark for term "),dy=d("weird error in injection subterms merge"),dz=[0,d("add_pacs")],dw=d(aU),dx=d("Updating term "),dt=d(aU),du=d(b$),dv=d("Merging "),dp=d(aU),dq=d(b$),dr=d("Linking "),dn=[0,d("plugins/cc/ccalgo.ml"),651,2],dg=d(aZ),dh=d(" <> "),di=d(b6),dj=d(b8),dk=d("Adding new disequality, depth="),db=d(aZ),dc=d(" == "),dd=d(b6),de=d(b8),df=d("Adding new equality, depth="),da=d("discarding redundant (dis)equality"),c8=d(aZ),c9=d(cd),c5=d(aZ),c6=d(":="),c7=d(cd),c4=d("incomplete matching"),cT=d("not a node"),cU=[0,d("subterms")],cK=d("not a constructor"),cL=[0,d("get_constructor")],cG=d("not a representative"),cH=[0,d("get_representative")],cw=d("signature already entered"),cx=[0,d("enter")],cn=[0,d(b_),[0,d("Verbose"),0]],co=d("Congruence Verbose"),cQ=d("Ccalgo.Discriminable"),cW=d(b3),cY=d(b3),dJ=d("_eps_"),dT=d("invalid cc transitivity"),dU=d("not enough args"),dV=[0,d("nth_arg")],dW=[0,1,20],dX=d("equal_proof "),dY=[0,1,20],dZ=d("edge_proof "),d0=[0,1,20],d1=d("constr_proof "),d3=d(","),d2=d("}"),d4=d("{"),d5=[0,1,20],d6=d("path_proof "),d7=[0,1,20],d8=d("congr_proof "),d9=[0,1,20],d_=d("ind_proof "),eD=d("f"),eE=d("I don't know how to handle dependent equality"),e1=[0,0],eY=d("congruence failed."),eZ=d(b_),eS=d("("),eT=d(")"),eO=d("Goal solved, generating proof ..."),eN=d("Computation completed."),eM=d("Problem built, solving ..."),eL=d("Reading subgoal ..."),eP=d("Goal is solvable by congruence but some arguments are missing."),eQ=d("  replacing metavariables by arbitrary terms."),eR=d(')",'),eU=d('"congruence with ('),eV=d("  Try "),eW=d("Incomplete"),eX=d("congruence failed"),eJ=d(cc),eK=d(ch),eI=d("H"),eG=d("e"),eH=d(cc),eF=d(ch),eB=[0,0],eC=[0,1],ez=d("t"),d$=d("CC"),ea=d(aS),eb=[0,d(W),[0,d(V),0]],ec=d("eq_rect"),ed=[0,d(W),[0,d(V),0]],ee=d("eq_refl"),ef=[0,d(W),[0,d(V),0]],eg=d("eq_sym"),eh=[0,d(W),[0,d(V),0]],ej=d("eq_trans"),ek=[0,d(W),[0,d(V),0]],el=d("eq"),em=[0,d(W),[0,d(V),0]],en=d("False"),eo=[0,d(W),[0,d(V),0]],ep=d("True"),eq=[0,d(W),[0,d(V),0]],er=d("I"),es=[0,d(W),[0,d(V),0]],fx=d(aS),fy=d(aS),fr=[0,d(aX),1,0],fn=[0,d(aX),1,0],fk=[0,d(aX),1,0],fh=[0,d(aX),1,0],fg=d(cl),fi=[0,d(cj)],fj=d(cf),fl=[0,d(aY)],fm=d(cl),fo=[0,d(cj)],fp=[0,d(aY)],fq=d(cf),fs=[0,d(aY)],ft=[0,[0,d(aY)],0],fu=d(cg),fa=d(aV),e_=d(aV),e8=d(aV),e6=d(aV),e4=d(b4),fe=d(cg),fA=d(aS),c3=h.Typeops,cq=h.Goptions,et=h.Inductiveops,e2=h.Type_errors,ey=h.Equality,ew=h.Context,ex=h.Evarutil,fv=h.Tacentries,fc=h.Array,ab=5;function
x(d){var
c=a1[1];if(c){var
e=a(d,0);return b(a2[16],0,e)}return c}function
cm(a){a1[1]=a;return 0}var
cp=[0,1,0,co,cn,function(a){return a1[1]},cm];b(cq[4],0,cp);var
cr=g[1],cs=[0,function(b,a){return b===a?1:0},cr],az=a(ao[18],cs);function
ct(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
cu=[0,ct,function(c){var
d=c[1],e=a(g[1],c[2]),f=a(g[1],d);return b(aj[2][1],f,e)}],ap=a(ao[18],cu);function
cv(f,e,d){if(b(ap[10],d[1],e)){var
g=a(c[1],cw);l(C[3],0,cx,g)}else
l(ap[9],d[1],e,f);return l(az[9],d[2],f,e)}function
cy(c,a){return b(ap[7],a[1],c)}function
cz(a,c){try{var
d=b(az[7],a[2],c);b(ap[6],a[1],d);var
e=b(az[6],a[2],c);return e}catch(a){a=r(a);if(a===s)return 0;throw a}}function
cA(c,a){function
d(a){return cz(c,a)}return b(g[2][13],d,a)}var
cB=[0,function(b,a){var
c=ax(b[1],a[1]),e=a[3],f=a[2],g=b[3],h=b[2];if(0===c){var
d=ax(h,f);return 0===d?l(i[17][45],ax,g,e):d}return c}],cC=[0,function(b,a){var
c=ax(b[1],a[1]),d=a[2],e=b[2];return 0===c?ax(e,d):c}],o=a(i[21][1],cB),n=a(i[21][1],cC);function
bl(b,a){var
c=0===b[0]?0===b[1]?0===a[0]?0===a[1]?1:0:0:0===a[0]?0===a[1]?0:1:0:0===a[0]?0:1;return c?1:0}function
a3(n,m){var
c=n,a=m;for(;;){switch(c[0]){case
0:if(0===a[0])return b(e[cb],c[1],a[1]);break;case
1:if(1===a[0]){var
o=a[2],p=c[2],g=bl(c[1],a[1]);return g?bl(p,o):g}break;case
2:if(2===a[0])return b(j[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],h=a3(c[1],a[1]);if(h){var
c=r,a=q;continue}return h}break;default:if(4===a[0]){var
d=a[1],f=c[1],i=f[2]===d[2]?1:0,s=d[3],t=d[1][1],u=f[3],v=f[1][1];if(i){var
k=u===s?1:0;if(k)return b(j[46],v,t);var
l=k}else
var
l=i;return l}}return 0}}function
a4(c){switch(c[0]){case
0:var
f=a(e[b7],c[1]);return b(aj[2][1],1,f);case
1:var
g=c[1],h=a(bm[6],c[2]),i=a(bm[6],g);return l(aj[2][3],2,i,h);case
2:var
k=a(j[1][3],c[1]);return b(aj[2][1],3,k);case
3:var
m=c[1],n=a4(c[2]),o=a4(m);return l(aj[2][3],4,o,n);default:var
d=c[1],p=d[3],q=d[2],r=a(j[50],d[1][1]);return B(aj[2][4],5,r,q,p)}}var
H=a(ao[18],[0,e[cb],e[b7]]),ac=a(ao[18],[0,a3,a4]),a5=a(ao[18],[0,j[1][1],j[1][3]]),cD=[0,a(e[G],I[8])],bn=[0,[1,I[8],[0,I[8],I[8],0]],I[8],o[1],0,cD];function
bo(c){var
b=a(ac[1],ab);return[0,ab,0,aQ(5,bn),a(H[1],ab),0,b]}function
cE(e,d){var
f=a(H[1],ab),h=a(a5[1],ab),i=g[2][1],j=a(t[2],0),k=a(t[2],0),l=g[2][1],b=a(az[1],ab),c=[0,a(ap[1],ab),b];return[0,bo(0),c,l,k,j,0,0,i,h,e,0,f,d]}function
cF(a){return a[1]}function
v(e,g){var
c=0,a=g;for(;;){var
d=q(e[3],a)[a+1][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
f=function(b){q(e[3],b)[b+1][2]=a;return 0};b(i[17][11],f,c);return a}}function
Q(e,b){var
d=q(e[3],b)[b+1][1];if(0===d[0])return d[1];var
f=a(c[1],cG);return l(C[3],0,cH,f)}function
a6(b,a){return q(b[3],a)[a+1][3]}function
cI(d,c,a){var
e=a6(d,c);return b(o[22],a,e)}function
cJ(c,f,e){var
a=f;for(;;)try{var
g=a6(c,a),h=b(o[22],e,g);return h}catch(b){b=r(b);if(b===s){var
d=q(c[3],a)[a+1][1];if(0===d[0])throw s;var
a=d[1];continue}throw b}}function
bp(e,b){var
d=q(e[3],b)[b+1][5];if(4===d[0])return d[1];var
f=a(c[1],cK);return l(C[3],0,cL,f)}function
bq(b,a){return Q(b,a)[1]}function
cM(a){return a[4]}function
cN(a){return a[5]}function
cO(e,d,c){var
a=Q(e,d);a[1]=a[1]+1|0;a[2]=b(g[2][4],c,a[2]);a[3]=b(g[2][4],c,a[3]);return 0}function
cP(e,d,c){var
a=Q(e,d);a[1]=a[1]+1|0;a[3]=b(g[2][4],c,a[3]);return 0}var
br=[248,cQ,w.caml_fresh_oo_id(0)];function
cR(b){var
c=a(i[17][4],b[3]);return[0,b[1],b[2]+1|0,c]}function
cS(a,c,e){try{var
i=b(n[22],c,a[6]),d=i}catch(a){a=r(a);if(a!==s)throw a;var
d=g[2][1]}var
f=a[6],h=b(g[2][4],e,d);a[6]=l(n[4],c,h,f);return 0}function
ak(b,a){return q(b[3],a)[a+1][5]}function
a7(f,b){var
d=q(f[3],b)[b+1][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[1],cT);return l(C[3],0,cU,g)}function
bs(a,c){var
b=a7(a,c),d=b[1],e=v(a,b[2]);return[0,v(a,d),e]}function
cV(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
d=((a[1]*3|0)/2|0)+1|0,e=aQ(d,bn);a[1]=d;fB(i[19][10],a[3],0,e,0,b);a[3]=e}a[2]=c;return b}function
aA(a){return[0,0,g[2][1],g[2][1],0,a,n[1]]}var
cX=[0,a(j[1][5],cW)],cZ=[0,a(j[1][5],cY)],c0=a(e[G],2),c1=[0,0,a(e[G],2),c0],c2=a(e[ci],c1);function
bt(g,c){var
h=a(e[P],g);if(10===h[0]){var
d=h[1],i=a(aB[2],0);if(b(aq[62],d[1],i)){var
f=b(j[aW][1],d[1],0);if(c){var
k=c[2],l=a(e[ai],[0,f,c[1]]);return b(e[60],l,k)}var
m=a(aB[2],0),n=b(c3[26],m,d)[1],o=a(aB[2],0),p=b(aq[61],f,o)[2]+1|0,q=b(e[85],p,n)[1],r=[0,f,a(e[G],1)],s=a(e[ai],r);return b(e[69],s,q)}}return b(e[60],g,c)}function
J(c){switch(c[0]){case
0:return bt(c[1],0);case
1:var
j=c[1],g=[0,cZ,a(e[bh],c[2]),c2],h=a(e[ah],g),i=[0,cX,a(e[bh],j),h];return a(e[ah],i);case
2:return a(e[aa],c[1]);case
3:var
k=c[1],f=[0,J(c[2]),0],d=k;for(;;){if(3===d[0]){var
l=d[1],f=[0,J(d[2]),f],d=l;continue}if(0===d[0])return bt(d[1],f);var
m=J(d);return b(e[60],m,f)}default:return a(e[bk],c[1][1])}}function
D(d){var
c=a(e[P],d);switch(c[0]){case
6:var
n=c[2],o=c[1],p=D(c[3]),q=[0,o,D(n),p];return a(e[ci],q);case
7:var
r=c[2],s=c[1],t=D(c[3]),u=[0,s,D(r),t];return a(e[ah],u);case
8:var
v=c[3],w=c[2],x=c[1],y=D(c[4]),z=D(v),B=[0,x,D(w),z,y];return a(e[123],B);case
9:var
C=c[1],E=b(i[19][51],D,c[2]),F=[0,D(C),E];return a(e[A],F);case
10:var
f=c[1],G=f[2],H=a(j[a0],f[1]),I=[0,a(j[aT],H),G];return a(e[b0],I);case
11:var
g=c[1],h=g[1],J=g[2],K=h[2],L=a(j[an],h[1]),M=[0,[0,a(j[ai],L),K],J];return a(e[an],M);case
12:var
k=c[1],l=k[1],m=l[1],N=k[2],O=l[2],Q=m[2],R=a(j[an],m[1]),S=[0,[0,[0,a(j[ai],R),Q],O],N];return a(e[bk],S);case
16:var
T=c[2],U=c[1],V=function(b){var
c=a(j[a0],b);return a(j[aT],c)},W=b(j[aW][10],V,U),X=[0,W,D(T)];return a(e[ai],X);default:return d}}function
a8(b,a){if(0===a[0]){var
d=a[2],e=a[1],f=function(c,a){return[3,a,a8(b,c)]};return l(i[17][16],f,d,e)}var
c=a[1]-1|0;return q(b,c)[c+1]}function
R(e,d){var
f=a(c[1],c5),g=J(ak(e,d)),h=a(S[5],g),i=a(c[1],c6),j=a(c[19],d),k=a(c[1],c7),l=b(c[13],k,j),m=b(c[13],l,i),n=b(c[13],m,h);return b(c[13],n,f)}function
aC(d){var
e=a(c[1],c8),f=J(d),g=a(S[5],f),h=a(c[1],c9),i=b(c[13],h,g);return b(c[13],i,e)}function
X(d,c){var
e=d[1];try{var
h=b(ac[7],e[6],c);return h}catch(h){h=r(h);if(h===s){var
a=cV(e),p=J(c),f=D(b(k[15],d[13],p));switch(c[0]){case
2:var
y=o[1],i=[0,[0,aA(f)],-1,y,0,c];break;case
3:var
z=c[2],m=X(d,c[1]),n=X(d,z);cO(e,v(e,m),a);cP(e,v(e,n),a);d[3]=b(g[2][4],a,d[3]);var
A=o[1],i=[0,[0,aA(f)],-1,A,[0,[0,m,n]],c];break;case
4:var
B=c[1];b(t[3],[0,a,[0,[0,a,0]]],d[5]);b(t[3],[0,a,[1,[0,a,B[2],0]]],d[5]);var
C=o[1],i=[0,[0,aA(f)],-1,C,0,c];break;default:b(t[3],[0,a,[0,[0,a,0]]],d[5]);var
u=o[1],i=[0,[0,aA(f)],-1,u,0,c]}q(e[3],a)[a+1]=i;l(ac[5],e[6],c,a);try{var
x=b(H[7],d[12],f),j=x}catch(a){a=r(a);if(a!==s)throw a;var
j=g[2][1]}var
w=b(g[2][4],a,j);l(H[9],d[12],f,w);return a}throw h}}function
bu(a,e,d,c){var
f=X(a,d),g=X(a,c);b(t[3],[0,f,g,[0,e,0]],a[4]);return l(H[5],a[1][4],e,[0,d,c])}function
bv(a,d,c,b){var
e=X(a,c),f=X(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
c_(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
c$(a,d,c){try{var
e=a[1],f=function(a){return v(e,a)},g=b(i[19][15],f,c),h=b(a5[8],a[9],d),j=function(b){function
c(c,b){return c===v(a[1],b)?1:0}return l(i[19][31],c,g,b)},k=b(i[17][23],j,h);return k}catch(a){a=r(a);if(a===s)return 0;throw a}}function
dl(e,b,a,d){var
c=q(e[3],b)[b+1];c[1]=[1,a,d];c[2]=a;return 0}function
bw(g,f,e){var
a=f,b=e;for(;;){var
c=q(g[3],a)[a+1][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
dm(c,i,h){var
o=v(c,h);if(v(c,i)===o){var
p=bw(c,h,0),a=[0,bw(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,ar,dn]}function
bx(d,h,j,w){x(function(o){var
e=a(c[1],dp),f=R(d[1],j),g=a(c[1],dq),i=R(d[1],h),k=a(c[1],dr),l=b(c[13],k,i),m=b(c[13],l,g),n=b(c[13],m,f);return b(c[13],n,e)});var
i=Q(d[1],h),e=Q(d[1],j);dl(d[1],h,j,w);try{var
G=b(H[7],d[12],i[5]),p=G}catch(a){a=r(a);if(a!==s)throw a;var
p=g[2][1]}var
y=b(g[2][6],h,p);l(H[9],d[12],i[5],y);var
u=b(g[2][7],i[3],e[3]);e[1]=a(g[2][20],u);e[3]=u;e[2]=b(g[2][7],i[2],e[2]);cA(d[2],i[3]);d[3]=b(g[2][7],d[3],i[3]);var
z=q(d[1][3],h)[h+1][3];function
A(c,a){return b(t[3],[0,a,[1,c]],d[5])}b(o[10],A,z);var
B=i[6];function
C(c){function
e(a){return b(t[3],[0,a,[0,c]],d[5])}return a(g[2][13],e)}b(n[10],C,B);var
k=i[4],f=e[4];if(typeof
k==="number"){if(0===k)return 0;if(typeof
f==="number"){if(0===f){e[4]=1;return 0}}else
if(0===f[0]){d[8]=b(g[2][6],j,d[8]);e[4]=1;return 0}}else
if(0===k[0]){var
D=k[1];if(typeof
f==="number"){if(0===f){e[4]=[0,D];d[8]=b(g[2][6],h,d[8]);d[8]=b(g[2][4],j,d[8]);return 0}var
v=0}else
var
v=1===f[0]?1:0;if(!v){d[8]=b(g[2][6],h,d[8]);return 0}}else{var
m=k[1],E=m[2],F=m[1];if(typeof
f==="number"){if(0===f){e[4]=[1,m];return 0}}else
if(0!==f[0])return b(t[3],[0,F,[1,E]],d[5])}return 0}function
ds(e,f){x(function(n){var
d=a(c[1],dt),g=R(f[1],e[2]),h=a(c[1],du),i=R(f[1],e[1]),j=a(c[1],dv),k=b(c[13],j,i),l=b(c[13],k,h),m=b(c[13],l,g);return b(c[13],m,d)});var
g=f[1],h=v(g,e[1]),i=v(g,e[2]),j=1-(h===i?1:0);if(j){var
l=bq(g,i);if(bq(g,h)<l)return bx(f,h,i,e);var
d=e[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bx(f,i,h,[0,e[2],e[1],k])}return j}function
dA(f,s,d){x(function(j){var
e=a(c[1],dB),g=R(d[1],f),h=a(c[1],dC),i=b(c[13],h,g);return b(c[13],i,e)});var
p=v(d[1],f),h=Q(d[1],p);if(0===s[0]){cS(h,s[1],f);d[3]=b(g[2][7],h[2],d[3]);return 0}var
e=s[1],r=q(d[1][3],p)[p+1];if(1-b(o[3],e,r[3]))r[3]=l(o[4],e,f,r[3]);var
i=h[4];if(typeof
i==="number"){if(0===i)return 0===e[2]?(h[4]=[1,[0,f,e]],0):(d[3]=b(g[2][7],h[2],d[3]),h[4]=[0,e],d[8]=b(g[2][4],p,d[8]),0)}else
if(1===i[0]){var
u=i[1],j=u[2],w=u[1];if(e[1]===j[1]){var
z=bp(d[1],e[1]),n=z[3],m=j[3],k=e[3];for(;;){var
y=0<n?1:0;if(y){if(m)if(k){var
A=k[2],B=m[2];b(t[3],[0,m[1],k[1],[1,w,j,f,e,n]],d[4]);var
n=n-1|0,m=B,k=A;continue}var
D=a(c[1],dy);return l(C[3],0,dz,D)}return y}}throw[0,br,w,j,f,e]}d[3]=b(g[2][7],h[2],d[3]);return 0}function
dD(e){var
g=e[1];function
h(f){if(f){var
d=f[1],k=f[2],l=v(g,d[2]);if(v(g,d[1])===l)var
j=a(c[1],dE),i=[0,d];else
var
m=h(k),j=a(c[1],dI),i=m;x(function(p){var
f=a(c[1],dF),g=R(e[1],d[2]),h=a(c[1],dG),i=R(e[1],d[1]),k=a(c[1],dH),l=b(c[13],k,i),m=b(c[13],l,h),n=b(c[13],m,g),o=b(c[13],n,f);return b(c[13],o,j)});return i}return 0}return h(e[6])}var
dK=a(j[1][5],dJ);function
dM(d){var
f=d[8];function
h(q){var
h=Q(d[1],q)[4];if(typeof
h!=="number"&&0===h[0]){var
f=h[1],v=J(ak(d[1],f[1])),w=b(k[15],d[13],v),x=f[3],y=function(a){return J(ak(d[1],a))},z=b(i[17][12],y,x),A=a(i[17][6],z),B=b(e[76],w,A),D=f[2],m=ak(d[1],q),o=B,j=D;for(;;){if(0<j){var
p=a(e[34],o),s=p[3],t=p[2],g=b(k[20],dK,d[13]),n=d[13];d[13]=l(by[4][12],n[2],n[1],[0,[0,g,t],0]);var
u=[0,a(e[aa],g),0],m=[3,m,[2,g]],o=b(bz[12],u,s),j=j-1|0;continue}d[1][5]=[0,f,d[1][5]];X(d,m);return 0}}var
r=a(c[1],dL);return l(C[3],0,0,r)}return b(g[2][13],h,f)}function
bA(c){var
a=[0,n[1]],f=c[1],d=c[1][3];function
e(c,h){var
d=c<f[2]?1:0;if(d){var
e=h[1];if(0===e[0]){var
i=e[1][6],j=function(d,j){try{var
i=b(n[22],d,a[1]),e=i}catch(a){a=r(a);if(a!==s)throw a;var
e=g[2][1]}var
f=a[1],h=b(g[2][4],c,e);a[1]=l(n[4],d,h,f);return 0};return b(n[10],j,i)}return 0}return d}b(i[19][14],e,d);return a[1]}function
bB(t,p,d){var
c=a(i[22][9],d),l=c[3];if(l){var
e=l[2],u=l[1],f=u[2],h=u[1],j=t[1];if(0===h[0]){var
k=h[2],m=h[1];if(k){var
A=k[2],B=k[1];try{var
C=b(ac[7],j[6],m),D=[0,C,a(i[17][1],k)],E=Q(j,f)[6],F=b(n[22],D,E),G=function(g){var
f=bs(t[1],g),h=[0,[0,[0,m,A],f[1]],[0,[0,B,f[2]],e]],j=c[2],k=[0,a(i[19][8],c[1]),j,h];return b(i[22][3],k,d)},H=b(g[2][13],G,F);return H}catch(a){a=r(a);if(a===s)return 0;throw a}}try{var
w=v(j,b(ac[7],j[6],m))===f?1:0,I=w?b(i[22][3],[0,c[1],c[2],e],d):w;return I}catch(a){a=r(a);if(a===s)return 0;throw a}}var
o=h[1],x=o-1|0;if(0<=q(c[1],x)[x+1]){var
y=o-1|0;return q(c[1],y)[y+1]===f?b(i[22][3],[0,c[1],c[2],e],d):0}var
z=o-1|0;q(c[1],z)[z+1]=f;return b(i[22][3],[0,c[1],c[2],e],d)}p[1]=[0,[0,c[2],c[1]],p[1]];return 0}function
a_(d,c){if(0===c[0]){var
e=c[1],f=a(i[17][1],c[2]);return[0,b(ac[7],d,e),f]}return a(I[1],dN)}function
bC(c){var
k=c[1][6],f=a(i[22][2],0),l=bA(c);function
d(a){var
h=a[5];if(typeof
h==="number")if(0===h)try{var
x=a_(k,a[4]),y=b(n[22],x,l),d=y}catch(a){a=r(a);if(a!==s)throw a;var
d=g[2][1]}else
var
d=g[2][1];else{var
z=h[1];try{var
A=b(H[7],c[12],z),o=A}catch(a){a=r(a);if(a!==s)throw a;var
o=g[2][1]}var
d=o}function
p(c){return b(i[22][3],[0,aQ(a[3],-1),a,[0,[0,a[4],c],0]],f)}b(g[2][13],p,d);var
j=a[7];if(typeof
j==="number")if(0===j)try{var
t=a_(k,a[6]),u=b(n[22],t,l),e=u}catch(a){a=r(a);if(a!==s)throw a;var
e=g[2][1]}else
var
e=g[2][1];else{var
v=j[1];try{var
w=b(H[7],c[12],v),m=w}catch(a){a=r(a);if(a!==s)throw a;var
m=g[2][1]}var
e=m}function
q(c){return b(i[22][3],[0,aQ(a[3],-1),a,[0,[0,a[6],c],0]],f)}return b(g[2][13],q,e)}b(i[17][11],d,c[7]);return f}function
bD(b){var
d=[0,0],e=bC(b);x(function(b){return a(c[1],dO)});try{for(;;){a(a9[2],0);bB(b,d,e);continue}}catch(a){a=r(a);if(a===i[22][1])return d[1];throw a}}function
a$(v,d){x(function(b){return a(c[1],dP)});try{for(;;){a(a9[2],0);try{ds(a(t[5],d[4]),d);var
I=1,h=I}catch(e){e=r(e);if(e!==t[1])throw e;try{var
u=a(t[5],d[5]);dA(u[1],u[2],d);var
H=1,h=H}catch(e){e=r(e);if(e!==t[1])throw e;try{var
f=a(g[2][24],d[3]);d[3]=b(g[2][6],f,d[3]);x(function(i){return function(j){var
e=a(c[1],dw),f=R(d[1],i),g=a(c[1],dx),h=b(c[13],g,f);return b(c[13],h,e)}}(f));var
k=bs(d[1],f),m=k[1],y=a7(d[1],f)[2],p=Q(d[1],m),q=p[4],X=typeof
q==="number"?0:0===q[0]?(p[4]=1,d[8]=b(g[2][6],m,d[8]),1):0,z=a6(d[1],m),B=function(c,e){return function(a,f){return b(t[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(y,f);b(o[10],B,z);var
D=p[6],E=function(c){return function(a,e){return b(t[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(f);b(n[10],E,D);try{var
F=cy(k,d[2]);b(t[3],[0,f,F,0],d[4])}catch(a){a=r(a);if(a!==s)throw a;cv(f,k,d[2])}var
G=1,h=G}catch(a){a=r(a);if(a!==s)throw a;var
h=0}}}if(h)continue;var
w=dD(d);if(w)var
P=w[1],T=v?[1,P]:0,j=[0,T];else
if(a(g[2][2],d[8]))if(0<d[10]){var
U=bD(d),V=function(p){var
m=p[2],f=p[1];a(a9[2],0);var
n=0<d[10]?1:0;if(n){if(c$(d,f[1],m))return x(function(b){return a(c[1],da)});l(a5[5],d[9],f[1],m);var
s=d[1],q=function(b){try{var
e=ak(s,b);return e}catch(b){b=r(b);if(a(C[22],b)){var
d=a(c[1],c4);return l(C[3],0,0,d)}throw b}},k=b(i[19][15],q,m),t=a(e[aa],f[1]),o=b(i[19][15],J,k);a(i[19][40],o);var
g=a(e[A],[0,t,o]),h=a8(k,f[4]),j=a8(k,f[6]);d[11]=1;d[10]=d[10]-1|0;return f[2]?(x(function(z){var
e=a(c[1],db),f=aC(j),i=a(c[1],dc),k=aC(h),l=a(c[1],dd),m=a(S[5],g),n=a(c[1],de),o=b(c[13],n,m),p=b(c[13],o,l),q=b(c[13],p,k),r=b(c[13],q,i),s=b(c[13],r,f),t=b(c[13],s,e),u=a(c[6],0),v=a(c[19],d[10]),w=a(c[1],df),x=b(c[13],w,v),y=b(c[13],x,u);return b(c[13],y,t)}),bu(d,g,h,j)):(x(function(z){var
e=a(c[1],dg),f=aC(j),i=a(c[1],dh),k=aC(h),l=a(c[1],di),m=a(S[5],g),n=a(c[1],dj),o=b(c[13],n,m),p=b(c[13],o,l),q=b(c[13],p,k),r=b(c[13],q,i),s=b(c[13],r,f),t=b(c[13],s,e),u=a(c[6],0),v=a(c[19],d[10]),w=a(c[1],dk),x=b(c[13],w,v),y=b(c[13],x,u);return b(c[13],y,t)}),bv(d,[0,g],h,j))}return n};b(i[17][11],V,U);var
W=d[11]?(d[11]=0,a$(1,d)):(x(function(b){return a(c[1],dQ)}),0),j=W}else{x(function(b){return a(c[1],dR)});var
j=0}else{x(function(b){return a(c[1],dS)});dM(d);var
j=a$(0,d)}return j}}catch(a){a=r(a);if(a[1]===br){var
K=a[5],L=a[4],M=a[3],N=a[2],O=v?[0,[0,N,M,L,K]]:0;return[0,O]}throw a}}var
f=[0,[0,n[1],n[2],n[3],n[4],n[5],n[6],n[7],n[8],n[9],n[10],n[11],n[12],n[13],n[14],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24]],[0,o[1],o[2],o[3],o[4],o[5],o[6],o[7],o[8],o[9],o[10],o[11],o[12],o[13],o[14],o[15],o[16],o[17],o[18],o[19],o[20],o[21],o[22],o[23],o[24]],H,ac,a3,J,x,cF,cM,cN,cE,X,bu,bv,c_,cR,v,cI,cJ,ak,bp,a7,dm,bA,bB,bC,a_,bD,a$,R,bo];ay(169,f,"Cc_plugin.Ccalgo");function
as(a){return[0,a,a,[2,a]]}function
al(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return as([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
y(o,n){var
e=o,d=n;for(;;){var
g=e[3],h=d[3];switch(g[0]){case
2:return d;case
4:var
k=g[2],m=g[1];switch(h[0]){case
2:var
i=0;break;case
3:var
j=h[1][3];if(4===j[0]){var
r=h[2],s=j[1],t=y(k,j[2]),e=al(y(m,s),t),d=r;continue}var
i=1;break;case
4:var
u=h[1],v=y(k,h[2]);return al(y(m,u),v);default:var
i=1}break;default:var
i=0}if(!i){if(2===h[0])return e;if(3===g[0]){var
q=g[1],e=q,d=y(g[2],d);continue}}if(b(f[5],e[2],d[1]))return[0,e[1],d[2],[3,e,d]];var
p=a(c[1],dT);return l(C[3],0,0,p)}}function
T(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=T(a[1]);return y(T(c),d);case
4:var
e=a[1],f=T(a[2]);return al(T(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,T(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
bE(d,a){var
c=b(f[3][7],d,a);return[0,c[1],c[2],[0,a]]}function
bF(d,a){var
c=b(f[3][7],d,a);return[0,c[2],c[1],[1,a]]}function
bG(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
h=b[2],i=b[1];if(0<d){var
b=i,d=d-1|0;continue}return h}var
g=a(c[1],dU);return l(C[3],0,dV,g)}}function
bH(c,d,b,a){var
e=bG(c[2],b-a|0);return[0,bG(c[1],b-a|0),e,[5,c,d,b,a]]}function
Y(d,e,g){function
i(n){var
h=b(f[30],d,g),i=a(c[3],dW),j=b(f[30],d,e),k=a(c[1],dX),l=b(c[13],k,j),m=b(c[13],l,i);return b(c[13],m,h)}a(f[7],i);if(e===g)return as(b(f[20],d,e));var
h=l(f[23],d,e,g),j=h[1],k=T(aD(d,g,h[2]));return y(aD(d,e,j),k)}function
bI(d,i){var
g=i[2],j=i[1],k=j[2],l=j[1];function
p(n){var
e=b(f[30],d,k),g=a(c[3],dY),h=b(f[30],d,l),i=a(c[1],dZ),j=b(c[13],i,h),m=b(c[13],j,g);return b(c[13],m,e)}a(f[7],p);var
q=Y(d,l,g[1]),r=T(Y(d,k,g[2])),e=g[3];if(typeof
e==="number")var
h=bJ(d,g[1],g[2]);else
if(0===e[0])var
m=e[1],s=e[2]?bF(a(f[9],d),m):bE(a(f[9],d),m),h=s;else
var
n=e[2],t=e[5],u=bb(d,e[1],n,e[3],e[4]),o=b(f[21],d,n[1]),h=bH(u,o[1],o[3],t);return y(y(q,h),r)}function
ba(d,g,e){function
k(k){var
e=a(c[3],d0),h=b(f[30],d,g),i=a(c[1],d1),j=b(c[13],i,h);return b(c[13],j,e)}a(f[7],k);var
h=l(f[19],d,g,e),i=Y(d,g,h);if(0===e[3])return i;var
m=a(f[16],e),j=b(f[22],d,h),n=j[1],o=b(f[20],d,j[2]),p=ba(d,n,m);return y(i,al(p,as(o)))}function
aD(e,g,d){function
i(u){var
h=a(c[1],d2);function
i(b){return a(c[19],b[1][2])}function
j(b){return a(c[1],d3)}var
k=l(c[53],j,i,d),m=a(c[1],d4),n=a(c[3],d5),o=b(f[30],e,g),p=a(c[1],d6),q=b(c[13],p,o),r=b(c[13],q,n),s=b(c[13],r,m),t=b(c[13],s,k);return b(c[13],t,h)}a(f[7],i);if(d){var
h=d[1],j=d[2],k=bI(e,h);return y(aD(e,h[1][2],j),k)}return as(b(f[20],e,g))}function
bJ(d,g,e){function
j(n){var
h=b(f[30],d,e),i=a(c[3],d7),j=b(f[30],d,g),k=a(c[1],d8),l=b(c[13],k,j),m=b(c[13],l,i);return b(c[13],m,h)}a(f[7],j);var
h=b(f[22],d,g),k=h[2],l=h[1],i=b(f[22],d,e),m=i[1],n=Y(d,k,i[2]);return al(Y(d,l,m),n)}function
bb(d,g,i,e,h){function
j(n){var
h=b(f[30],d,e),i=a(c[3],d9),j=b(f[30],d,g),k=a(c[1],d_),l=b(c[13],k,j),m=b(c[13],l,i);return b(c[13],m,h)}a(f[7],j);var
k=Y(d,g,e),l=ba(d,g,i),m=y(k,ba(d,e,h));return y(T(l),m)}var
aE=[0,as,al,y,T,bE,bF,bH,Y,bI,aD,bJ,bb,function(c,b){if(b1<=b[1]){var
a=b[2];return bb(c,a[1],a[2],a[3],a[4])}var
d=b[2];return Y(c,d[1],d[2])}];ay(170,aE,"Cc_plugin.Ccproof");function
U(b,a){return[F,function(c){return l(bc[5],d$,b,a)}]}var
aF=U(eb,ea),bK=U(ed,ec),bL=U(ef,ee),ei=U(eh,eg),bM=U(ek,ej),u=U(em,el),Z=U(eo,en),aG=U(eq,ep),aH=U(es,er);function
bN(c){var
d=l(_[42],0,_[14],c);return function(c){var
e=a(_[36],c);return b(_[47],d,e)}}function
aI(c){var
d=l(_[42],0,_[9],c);return function(c){var
e=a(_[36],c);return b(_[47],d,e)}}function
aJ(c,b,a){return l(at[4],c,[0,b],a)}function
z(d,f,h){var
y=a(bN(d),h),c=a(e[P],y);switch(c[0]){case
6:var
m=c[3],n=c[2],A=a(e[G],1);if(!b(S[45],A,m)){var
o=a(S[56],m),B=aJ(d,f,o),C=aJ(d,f,n),D=z(d,f,o);return[3,[3,[1,C,B],z(d,f,n)],D]}break;case
9:var
E=c[2],F=z(d,f,c[1]),H=function(a){return z(d,f,a)},I=b(i[19][15],H,E),J=function(b,a){return[3,b,a]};return l(i[19][17],J,F,I);case
10:var
p=c[1],K=p[2],L=a(j[a0],p[1]),M=[0,a(j[aT],L),K];return[0,a(e[b0],M)];case
11:var
q=c[1],r=q[1],N=q[2],O=r[2],Q=a(j[an],r[1]),R=[0,[0,a(j[ai],Q),O],N];return[0,a(e[an],R)];case
12:var
t=c[1],u=t[1],v=u[2],w=u[1],T=t[2],U=w[2],V=a(j[an],w[1]),g=[0,a(j[ai],V),U],W=a(aB[26],g)[1],x=b(et[44],d,[0,g,v]);return[4,[0,[0,[0,g,v],T],x,x-W[6]|0]];case
16:var
X=c[2],Y=c[1],Z=function(b){var
c=a(j[a0],b);return a(j[aT],c)},_=b(j[aW][10],Z,Y),$=z(d,f,X),aa=a(j[aW][3],_);return[3,[0,a(e[b9],aa)],$]}var
k=a(e[99],h);if(a(bz[2],k))return[0,k];throw s}function
bd(c,d,g){var
j=a(aI(c),g),h=a(e[P],j);if(9===h[0]){var
f=h[2],i=N(u),k=h[1],l=O===i?u[1]:F===i?a(K[2],u):u;if(b(au[11],l,k))if(3===f.length-1){var
m=z(c,d,q(f,2)[3]),n=z(c,d,q(f,1)[2]);return[0,aR,[0,q(f,0)[1],n,m]]}return[0,bj,z(c,d,g)]}return[0,bj,z(c,d,g)]}function
av(c,d,h){var
r=a(bN(c),h),f=a(e[P],r);switch(f[0]){case
0:var
j=f[1];return[0,[1,j],a(g[2][5],j)];case
6:var
k=f[3],m=f[2],t=a(e[G],1);if(!b(S[45],t,k)){var
n=a(S[56],k),o=av(c,d,m),u=o[2],v=o[1],p=av(c,d,n),w=p[2],x=p[1],y=aJ(c,d,n),A=aJ(c,d,m);return[0,[0,[1,A,y],[0,v,[0,x,0]]],b(g[2][7],u,w)]}break;case
9:var
B=f[2],C=z(c,d,f[1]),D=function(a){return av(c,d,a)},E=b(i[19][48],D,B),q=a(i[17][38],E),F=q[1],H=l(i[17][15],g[2][7],g[2][1],q[2]);return[0,[0,C,a(i[17][6],F)],H]}var
s=z(c,d,h);return[0,[0,s,0],g[2][1]]}function
bO(a){return 0===a[0]?1:0}function
bP(f,h,d,t){try{var
v=a(aI(f),t),i=a(e[37],v)}catch(a){a=r(a);if(a===e[28])throw s;throw a}var
c=i[2],j=N(u),w=i[1],x=O===j?u[1]:F===j?a(K[2],u):u;if(b(au[11],x,w))if(3===c.length-1){var
k=av(f,h,q(c,1)[2]),l=k[1],y=k[2],m=av(f,h,q(c,2)[3]),n=m[1],z=m[2],o=a(g[2][20],y)===d?bO(l)?0:[0,q(c,0)[1]]:1,p=a(g[2][20],z)===d?bO(n)?0:[0,q(c,0)[1]]:1;if(1===o)if(1===p)throw s;return[0,d,o,l,p,n]}throw s}function
eu(n,h,m,l){var
c=n,d=m,g=l;for(;;){var
o=a(aI(c),g),f=a(e[P],o);if(6===f[0]){var
i=f[3],j=f[2],k=N(Z),p=f[1],q=O===k?Z[1]:F===k?a(K[2],Z):Z;if(b(au[11],q,i))return[0,ca,bP(c,h,d,j)];var
c=b(aq[20],[0,p,j],c),d=d+1|0,g=i;continue}return[0,b2,bP(c,h,d,g)]}}function
ev(c,d,g){var
m=a(aI(c),g),f=a(e[P],m);if(6===f[0]){var
j=f[3],k=f[2],l=N(Z),n=f[1],o=O===l?Z[1]:F===l?a(K[2],Z):Z;if(b(au[11],o,j)){var
h=bd(c,d,k);if(aR<=h[1]){var
i=h[2];return[0,b5,[0,i[1],i[2],i[3]]]}return[0,ck,h[2]]}try{var
p=eu(b(aq[20],[0,n,k],c),d,1,j);return p}catch(a){a=r(a);if(a===s)return[0,bj,z(c,d,g)];throw a}}return bd(c,d,g)}function
bQ(d,h,g,f,c){var
i=h[1][2],l=a(e[G],1),m=a(k[2],c),n=a(k[8],c),o=bg(ey[38],n,m,i,l,d,g,f),p=a(j[1][5],ez),q=[0,[0,b(k[20],p,c)],d,o];return a(e[ah],q)}var
$=e[114];function
ad(c,g,f){var
d=N(c);function
h(b){return a(f,a(e[A],[0,b,g]))}var
i=O===d?c[1]:F===d?a(K[2],c):c;return b(p[67],i,h)}function
ae(c,g,f){var
d=N(c);function
h(b){return a(f,a(e[A],[0,b,g]))}var
i=O===d?c[1]:F===d?a(K[2],c):c;return b(p[70][57],i,h)}function
eA(b){var
c=a(k[45],b);return a(m[66][1],c)}function
aL(d,c){var
e=[0,function(e){function
f(a){return b(at[2],0,a)}var
g=l(k[48][1],f,e,c)[1],h=b(E[138],d,c),i=a(aM[11],g),j=a(m[66][1],i);return b(p[70][3],j,h)}];return a(m[62][10],e)}function
bR(c,b,a){return bg(bS[4],[0,aK[ce]],0,eC,eB,c,b,a)}function
L(f,e){var
c=[0,function(c){var
g=a(m[62][5],c),d=bR(g,a(k[48][4],c),f),h=d[1],i=a(e,d[2]),j=a(aM[11],h),l=a(m[66][1],j);return b(p[70][3],l,i)}];return a(m[62][10],c)}function
M(l){var
d=[0,function(q){function
g(a){return b(k[48][7],q,a)}try{var
d=l[3];switch(d[0]){case
0:var
h=a(E[45],d[1]);break;case
1:var
F=d[1],w=a(f[6],l[1]),H=a(f[6],l[2]),I=function(a){return ae(ei,[0,a,H,w,F],E[45])},h=L(g(w),I);break;case
2:var
x=d[1],J=a(f[6],x),K=function(b){var
c=E[45];return ae(bL,[0,b,a(f[6],x)],c)},h=L(g(J),K);break;case
3:var
y=d[2],s=d[1],N=a(f[6],s[1]),z=a(f[6],s[2]),O=a(f[6],y[2]),P=function(c){var
d=a($,2),e=[0,c,N,z,O,a($,1),d],f=[0,M(y),0],g=[0,M(s),f],h=ae(bM,e,eA);return b(p[70][19],h,g)},h=L(g(z),P);break;case
4:var
t=d[2],u=d[1],n=a(f[6],u[1]),i=a(f[6],t[1]),o=a(f[6],u[2]),B=a(f[6],t[2]),Q=function(f){function
d(h){function
d(d){function
g(c){var
d=a(j[1][5],eD);return b(k[20],d,c)}var
l=b(k[48][3],g,q),r=[0,a(e[G],1),[0,i]],s=[0,[0,l],f,a(e[A],r)],v=a(e[ah],s),w=[0,f,d,v,n,o,a($,1)],x=[0,h,d,o,i,B,a($,1)],y=a($,3),z=a($,2),C=a(e[A],[0,o,[0,B]]),D=a(e[A],[0,o,[0,i]]),F=[0,d,a(e[A],[0,n,[0,i]]),D,C,z,y],H=a(c[1],eE),I=[0,b(p[70][5],0,H),0],J=[0,E[A],I],K=M(t),U=ad(aF,x,k[45]),L=a(m[66][1],U),N=[0,b(p[70][3],L,K),J],O=[0,a(p[70][23],N),0],P=M(u),V=ad(aF,w,k[45]),Q=a(m[66][1],V),R=[0,b(p[70][3],Q,P),O],T=ad(bM,F,k[45]),S=a(m[66][1],T);return b(p[70][19],S,R)}return L(g(a(e[A],[0,n,[0,i]])),d)}return L(g(i),d)},h=L(g(n),Q);break;default:var
v=d[1],R=d[4],S=d[3],T=d[2],C=a(f[6],v[1]),U=a(f[6],v[2]),D=a(f[6],l[1]),V=a(e[G],(1+S|0)-R|0),W=function(c){function
d(d){function
e(a){return bQ(c,T,V,D,a)}var
f=b(k[48][3],e,q),g=[0,c,d,f,C,U,a($,1)],h=M(v),j=ad(aF,g,k[45]),i=a(m[66][1],j);return b(p[70][3],i,h)}return L(g(D),d)},h=L(g(C),W)}return h}catch(c){c=r(c);if(a(m[66][10],c))return b(m[18],0,c);throw c}}];return a(m[62][9],d)}function
bT(d,c){function
e(a){return b(at[2],0,a)}var
f=l(k[23],e,c,d)[1],g=a(E[45],d),h=a(m[66][8],g),i=a(aM[11],f);return l(p[5],i,h,c)}function
bU(n,l,h,i){var
c=[0,function(c){var
g=a(f[6],l),d=a(f[6],h);function
o(f){var
o=a(j[1][5],eG),q=a(k[20],o),h=b(k[48][3],q,c),r=a(j[1][5],eH),s=a(k[20],r),t=b(k[48][3],s,c),v=[0,[0,t],f,a(e[G],1)],w=a(e[ah],v),A=ad(bK,[0,f,g,w,n,d,a(e[aa],h)],bT),x=[0,a(m[66][1],A),0],l=[0,f,g,d],y=[0,M(i),x],z=[0,h],B=ae(u,l,function(a){return aL(z,a)});return b(p[70][19],B,y)}return L(b(k[48][7],c,d),o)}];return a(m[62][9],c)}function
bW(P,J){var
d=[0,function(v){a(bc[11],bc[14]);function
Q(b){return a(c[1],eL)}a(f[7],Q);function
R(d){var
m=a(k[8],d),j=a(aK[69],d),c=b(f[11],P,d),l=[0,0],n=[0,0];function
o(a){var
d=z(m,j,a);b(f[12],c,d);return 0}b(i[17][11],o,J);var
p=b(by[4][3],d[2],d[1]),q=a(aq[26],p);function
r(t){var
o=a(ew[2][1][17],t),k=o[1],u=o[3],g=a(e[aa],k),d=ev(m,j,u),h=d[1];if(aR<=h){if(ca<=h)return b2<=h?B(f[15],c,k,1,d[2]):B(f[15],c,k,0,d[2]);if(b5<=h){var
p=d[2];return B(f[14],c,[0,g],p[2],p[3])}var
q=d[2];return B(f[13],c,g,q[2],q[3])}if(ck<=h){var
r=d[2],v=l[1],w=function(a){return B(f[14],c,[2,a[1],g],a[2],r)};b(i[17][11],w,v);n[1]=[0,[0,g,r],n[1]];return 0}var
s=d[2],x=n[1];function
y(a){return B(f[14],c,[2,g,a[1]],s,a[2])}b(i[17][11],y,x);l[1]=[0,[0,g,s],l[1]];return 0}b(i[17][11],r,q);var
s=a(k[7],d),g=bd(m,j,b(ex[32],j,s));if(aR<=g[1]){var
h=g[2];B(f[14],c,0,h[2],h[3])}else{var
t=g[2],u=l[1],v=function(a){return B(f[14],c,[1,a[1]],a[2],t)};b(i[17][11],v,u)}return c}var
w=b(k[48][3],R,v);function
T(b){return a(c[1],eM)}a(f[7],T);var
x=b(f[29],1,w);function
U(b){return a(c[1],eN)}a(f[7],U);var
d=a(f[8],w);if(x){var
n=x[1],V=function(b){return a(c[1],eO)};a(f[7],V);if(typeof
n==="number"){var
y=[0,0],W=a(m[62][5],v),X=function(b){y[1]++;return a($,y[1])},Y=a(f[10],d),Z=function(c){var
g=b(f[21],d,c[1]),h=c[3];function
j(c){var
e=b(f[20],d,c);return a(f[6],e)}var
k=b(i[17][12],j,h),l=b(i[17][48],c[2],X),m=a(i[17][6],l),n=b(i[17][8],k,m),o=a(e[bk],g[1]);return b(e[60],o,n)},_=b(i[17][12],Z,Y),ab=a(c[1],eP);b(a2[12],0,ab);var
ac=a(c[1],eQ),af=a(c[1],eR),ag=a(S[6],W),ai=function(h){var
d=a(c[1],eS),e=a(c[16],0),f=a(c[1],eT),g=b(c[13],f,e);return b(c[13],g,d)},aj=l(c[53],ai,ag,_),ak=a(c[1],eU),al=b(c[13],ak,aj),am=b(c[13],al,af),an=b(c[29],8,am),ao=a(c[1],eV),ap=b(c[13],ao,an),ar=b(c[13],ap,ac);b(a2[12],0,ar);var
as=a(c[1],eW);return b(p[70][4],0,as)}else{if(0===n[0]){var
o=n[1],C=o[2],s=b(aE[13],d,[0,b1,[0,o[1],C,o[3],o[4]]]),at=b(f[21],d,C[1])[1],I=[0,function(c){var
d=a(f[6],s[1]),i=a(f[6],s[2]),w=a(m[62][5],c),l=a(m[62][3],c),x=a(j[1][5],eJ),y=a(k[20],x),n=N(aH),z=b(k[48][3],y,c),A=O===n?aH[1]:F===n?a(K[2],aH):aH,o=N(aG),C=a(bV[48],A),D=O===o?aG[1]:F===o?a(K[2],aG):aG,q=a(bV[48],D),E=a(k[48][4],c),r=bR(w,E,b(k[48][7],c,d)),g=r[2],t=B(aK[b9],0,0,aK[ce],r[1]),H=t[1],h=a(e[bh],t[2]),I=[0,[0,z],h,a(e[G],1)],J=a(e[ah],I),L=a(j[1][5],eK),P=a(k[20],L),v=b(k[48][3],P,c);function
Q(a){return bQ(g,at,q,l,a)}var
R=b(k[48][3],Q,c),S=[0,g,h,R,d,i,a(e[aa],v)],T=[0,g,d,i],U=0,$=ad(aF,S,function(a){return ad(bK,[0,h,q,J,C,l,a],bT)}),V=[0,a(m[66][1],$),U],W=[0,M(s),V],X=[0,v],_=ae(u,T,function(a){return aL(X,a)}),Y=b(p[70][19],_,W),Z=a(m[60][1],H);return b(p[70][3],Z,Y)}];return a(m[62][9],I)}var
h=n[1],q=b(aE[13],d,[0,-608347012,[0,h[1],h[2]]]),t=b(f[20],d,h[1]),r=b(f[20],d,h[2]),g=h[3];if(typeof
g==="number")return M(q);else
switch(g[0]){case
0:var
au=g[1],D=[0,function(c){var
d=a(f[6],t),h=a(f[6],r),i=a(j[1][5],eF),l=a(k[20],i),g=b(k[48][3],l,c),m=[0,au,[0,a(e[aa],g)]],n=a(e[A],m);function
o(c){var
f=[0,a(E[99],n),0],e=[0,c,d,h],i=[0,M(q),f],j=[0,g],k=ae(u,e,function(a){return aL(j,a)});return b(p[70][19],k,i)}return L(b(k[48][7],c,d),o)}];return a(m[62][9],D);case
1:return bU(g[1],t,r,q);default:var
av=g[2],aw=g[1],H=[0,function(d){var
g=a(f[6],r),h=a(j[1][5],eI),i=a(k[20],h),c=b(k[48][3],i,d),l=[0,av,[0,a(e[aa],c)]],m=a(e[A],l),n=[0,a(E[99],m),0],o=[0,bU(aw,t,r,q),n],s=aL([0,c],g);return b(p[70][19],s,o)}];return a(m[62][9],H)}}}var
ax=a(c[1],eX);return b(p[70][4],0,ax)}];return a(m[62][9],d)}function
bX(e){var
d=a(c[1],eY);return b(C[7],eZ,d)}function
bY(d,c){var
e=a(m[66][1],bX),f=bW(d,c),g=a(p[70][28],E[17]),h=b(p[70][3],g,f);return b(p[70][12],h,e)}function
e0(c,d,j,i){var
f=N(c);function
g(n){var
c=[0,function(c){function
o(a){return b(at[2],0,a)}var
f=l(k[48][1],o,c,d),q=f[2],r=f[1],s=a(k[48][5],c),g=bg(bS[4],0,0,0,e1,s,r,q),t=g[1],h=a(e[A],[0,n,[0,g[2],d,j]]),u=a(k[48][5],c),v=B(at[2],0,u,t,h)[1],w=a(i,h),x=a(aM[11],v),y=a(m[66][1],x);return b(p[70][3],y,w)}];return a(m[62][10],c)}var
h=O===f?c[1]:F===f?a(K[2],c):c;return b(p[70][57],h,g)}var
e3=[0,function(t){var
v=a(m[62][3],t);function
w(d,c){try{var
e=0,f=E[85],g=[0],h=function(a){return ae(bL,g,a)}(f),i=[0,a(p[70][22],h),e],j=[0,a(m[13],0),i],k=e0(u,d,c,E[145]),l=b(p[70][19],k,j);return l}catch(c){c=r(c);if(a(m[66][10],c))return b(m[18],0,c);throw c}}function
x(c){var
d=c[1],e=c[2];return d[1]===e2[1]?a(m[13],0):b(m[18],[0,e],d)}var
f=a(e[P],v);if(9===f[0]){var
g=f[2];if(3===g.length-1){var
j=N(u),y=f[1],z=g[2],A=g[3],B=O===j?u[1]:F===j?a(K[2],u):u;if(b(au[11],B,y)){var
k=a(e[P],z),l=a(e[P],A);if(9===k[0])if(9===l[0]){var
o=l[2],h=k[2];if(h.length-1===o.length-1)var
s=function(c){if(0<=c){var
d=s(c-1|0),e=q(o,c)[c+1],f=w(q(h,c)[c+1],e);return b(p[70][16],f,d)}var
g=bY(bi,0);return a(p[70][22],g)},n=s(h.length-1-1|0),d=1;else
var
d=0}else
var
d=0;else
var
d=0;if(!d)var
n=a(m[13],0);var
i=n,c=1}else
var
c=0}else
var
c=0}else
var
c=0;if(!c)var
i=a(m[13],0);return b(m[20],i,x)}],af=[0,M,bW,bX,bY,a(m[62][9],e3)];ay(188,af,"Cc_plugin.Cctac");a(be[12],e4);var
e5=0,e7=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(am[6],aN[6]),h=b(aO[2][7],g,f),i=a(am[17],aP[8]),j=a(am[6],i),k=b(aO[2][7],j,e);return function(a){return b(af[4],h,k)}}}return a(I[2],e6)},e5],e9=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(am[17],aP[8]),f=a(am[6],e),g=b(aO[2][7],f,d);return function(a){return b(af[4],bi,g)}}return a(I[2],e8)},e7],e$=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(am[6],aN[6]),f=b(aO[2][7],e,d);return function(a){return b(af[4],f,0)}}return a(I[2],e_)},e9],fb=[0,function(c){return c?a(I[2],fa):function(a){return b(af[4],bi,0)}},e$],fd=a(fc[12],fb);l(bf[9],0,[0,ag,fe],fd);function
ff(s){var
i=a(j[1][6],fg),c=aP[8],g=0,h=0;if(0===c[0]){var
k=[0,fi,[0,[1,aw[4],[0,[5,[0,c[1]]]],i],h]],l=a(j[1][6],fj),d=aN[6];if(0===d[0]){var
m=[0,[0,fl,[0,[1,aw[4],[5,[0,d[1]]],l],k]],g],o=a(j[1][6],fm),e=aP[8],n=0;if(0===e[0]){var
p=[0,[0,fp,[0,fo,[0,[1,aw[4],[0,[5,[0,e[1]]]],o],n]]],m],r=a(j[1][6],fq),f=aN[6],q=0;if(0===f[0])return b(fv[4],[0,ag,fu],[0,ft,[0,[0,fs,[0,[1,aw[4],[5,[0,f[1]]],r],q]],p]]);throw[0,ar,fr]}throw[0,ar,fn]}throw[0,ar,fk]}throw[0,ar,fh]}b(be[19],ff,ag);function
fw(d){var
b=[28,[0,0,[31,aw[4],[0,[0,ag,fx],0],0]]],c=a(j[1][5],fy);return B(bf[4],1,0,c,b)}var
fz=[0,function(b,a){return af[5]}];l(bf[9],0,[0,ag,fA],fz);b(be[19],fw,ag);var
bZ=[0,ag];ay(198,bZ,"Cc_plugin.G_congruence");ay(199,[0,f,aE,af,bZ],"Cc_plugin");return});
