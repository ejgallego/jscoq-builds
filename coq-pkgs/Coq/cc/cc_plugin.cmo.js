function(fg){"use strict";var
am=104,aE=".",bX=108,bY="$l",bP="  [",bN=3901498,bO=" : ",bW=-431191102,bT="$n",a0=-912009552,T=250,_="Init",aD=124,bM=127,N=246,aF="congruence",bS="[",bL="A",bV="with",aH="]",bK=915186972,bR=888453194,Z="Logic",bQ=" and ",a1=109,bJ=-318868643,aG=107,bU="Heq",aY="f_equal",aC=15500,aZ=1e3,x=fg.jsoo_runtime,r=x.caml_check_bound,ak=x.caml_int_compare,aA=x.caml_make_vect,d=x.caml_new_string,S=x.caml_obj_tag,al=x.caml_register_global,s=x.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):x.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):x.caml_call_gen(a,[b,c])}function
j(a,b,c,d){return a.length==3?a(b,c,d):x.caml_call_gen(a,[b,c,d])}function
z(a,b,c,d,e){return a.length==4?a(b,c,d,e):x.caml_call_gen(a,[b,c,d,e])}function
bI(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):x.caml_call_gen(a,[b,c,d,e,f])}function
aB(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):x.caml_call_gen(a,[b,c,d,e,f,g,h])}function
ff(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):x.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}var
h=x.caml_get_global_data(),aw=d("cc_plugin"),o=h.Constr,l=h.Names,$=h.Hashset,a3=h.Sorts,i=h.Util,t=h.Not_found,ab=h.Term,ap=h.Global,a$=h.Environ,e=h.EConstr,p=h.Tacmach,g=h.Int,v=h.Queue,c=h.Pp,aQ=h.Control,E=h.CErrors,be=h.Assert_failure,C=h.Termops,aJ=h.Feedback,ae=h.Hashtbl,ag=h.Pervasives,R=h.CamlinternalLazy,q=h.Tacticals,k=h.Proofview,F=h.Tactics,ad=h.Typing,bA=h.Evarsolve,bD=h.DAst,aV=h.Coqlib,by=h.Equality,bx=h.Context,bt=h.CClosure,bu=h.Reductionops,ax=h.Stdarg,ay=h.Genarg,az=h.Loc,bG=h.Ltac_plugin,aI=[0,0],du=d("Out of depth ... "),dt=d("Out of instances ... "),dv=d("First run was incomplete, completing ... "),ds=d("Executing ... "),dr=d("Running E-matching algorithm ... "),dq=d("paf_of_patt: pattern is trivial"),dm=d("wrong incomplete class."),df=d(" ... "),dg=d(" = "),dh=d("Checking if "),de=d("Yes"),di=d("No"),db=d(aE),dc=d("Processing mark for term "),c_=d("weird error in injection subterms merge."),c$=[0,d("add_pacs")],c8=d(aE),c9=d("Updating term "),c5=d(aE),c6=d(bQ),c7=d("Merging "),c1=d(aE),c2=d(bQ),c3=d("Linking "),c0=[0,d("plugins/cc/ccalgo.ml"),654,2],cT=d(aH),cU=d(" <> "),cV=d(bO),cW=d(bP),cX=d("Adding new disequality, depth="),cO=d(aH),cP=d(" == "),cQ=d(bO),cR=d(bP),cS=d("Adding new equality, depth="),cN=d("discarding redundant (dis)equality"),cJ=d(aH),cK=d(bS),cG=d(aH),cH=d(":="),cI=d(bS),cF=d("incomplete matching."),cu=d("not a node."),cv=[0,d("subterms")],cl=d("not a constructor."),cm=[0,d("get_constructor")],ch=d("not a representative."),ci=[0,d("get_representative")],b9=d("signature already entered."),b_=[0,d("enter")],b0=[0,d("Congruence"),[0,d("Verbose"),0]],b1=d("Congruence Verbose"),cr=d("Ccalgo.Discriminable"),cx=d(bL),cz=d(bL),dj=d("_eps_"),dw=d("invalid cc transitivity."),dx=d("not enough args."),dy=[0,d("nth_arg")],dz=[0,1,20],dA=d("equal_proof "),dB=[0,1,20],dC=d("edge_proof "),dD=[0,1,20],dE=d("constr_proof "),dG=d(","),dF=d("}"),dH=d("{"),dI=[0,1,20],dJ=d("path_proof "),dK=[0,1,20],dL=d("congr_proof "),dM=[0,1,20],dN=d("ind_proof "),d_=[0,d("plugins/cc/cctac.ml"),254,9],ee=d("f"),ef=d("I don't know how to handle dependent equality"),eG=[0,0],ex=d("("),ey=d(")"),er=[0,1],ep=d("Goal solved, generating proof ..."),eo=d("Computation completed."),en=d("Problem built, solving ..."),em=d("Reading subgoal ..."),eq=[13,0,0,0],eu=d("Goal is solvable by congruence but some arguments are missing."),ev=d("  replacing metavariables by arbitrary terms."),ew=d(')",'),ez=d('"congruence with ('),eA=d("  Try "),eB=d("Incomplete"),eC=d("congruence failed"),el=d(bU),ek=d("H"),ei=d("e"),ej=d("X"),eg=d(bU),eb=[0,0],ec=[0,1],d9=d("t"),dO=d("CC"),dP=d(aY),dQ=[0,d(_),[0,d(Z),0]],dR=d("eq_rect"),dS=[0,d(_),[0,d(Z),0]],dU=d("eq_refl"),dV=[0,d(_),[0,d(Z),0]],dW=d("eq_sym"),dX=[0,d(_),[0,d(Z),0]],dZ=d("eq_trans"),d0=[0,d(_),[0,d(Z),0]],d1=d("eq"),d2=[0,d(_),[0,d(Z),0]],d3=d("False"),d4=[0,d(_),[0,d(Z),0]],eD=d("congruence failed."),eN=d(bY),eQ=d(bV),eS=d(bT),eV=d(aF),eY=d(bY),e1=d(bV),e2=d(aF),e5=d(bT),e8=d(aF),e_=[0,d(aF),0],fa=d("cc"),fc=[0,d(aY),0],fe=d(aY),cE=h.Typeops,dn=h.Vars,dl=h.Goal,b3=h.Goptions,d5=h.Inductiveops,d6=h.Retyping,d$=h.Evarutil,eI=h.Pretype_errors,eJ=h.Type_errors,es=h.Detyping,et=h.Printer,ed=h.Evd,ea=h.Refine,eK=h.Mltop,U=5;function
A(d){var
c=aI[1];if(c){var
e=a(d,0);return b(aJ[10],0,e)}return c}function
bZ(a){aI[1]=a;return 0}var
b2=[0,0,b1,b0,function(a){return aI[1]},bZ];b(b3[4],0,b2);var
b4=g[1],b5=[0,function(b,a){return b===a?1:0},b4],an=a(ae[19],b5);function
b6(b,a){var
c=b[1]===a[1]?1:0,d=a[2],e=b[2],f=c?e===d?1:0:c;return f}var
b7=[0,b6,function(c){var
d=c[1],e=a(g[1],c[2]),f=a(g[1],d);return b($[2][1],f,e)}],af=a(ae[19],b7);function
b8(f,e,d){if(b(af[11],d[1],e)){var
g=a(c[3],b9);j(E[3],0,b_,g)}else
j(af[10],d[1],e,f);return j(an[10],d[2],f,e)}function
b$(c,a){return b(af[7],a[1],c)}function
ca(a,c){try{var
d=b(an[7],a[2],c);b(af[6],a[1],d);var
e=b(an[6],a[2],c);return e}catch(a){a=s(a);if(a===t)return 0;throw a}}function
cb(c,a){function
d(a){return ca(c,a)}return b(g[2][13],d,a)}var
cc=[0,function(b,a){var
c=ak(b[1],a[1]),e=a[3],f=a[2],g=b[3],h=b[2];if(0===c){var
d=ak(h,f);return 0===d?j(i[17][51],ak,g,e):d}return c}],cd=[0,function(b,a){var
c=ak(b[1],a[1]),d=a[2],e=b[2];return 0===c?ak(e,d):c}],n=a(i[21][1],cc),m=a(i[21][1],cd);function
a2(b,a){var
c=0===b[0]?0===b[1]?0===a[0]?0===a[1]?1:0:0:0===a[0]?0===a[1]?0:1:0:0===a[0]?0:1;return c?1:0}function
aK(m,k){var
c=m,a=k;for(;;){switch(c[0]){case
0:if(0===a[0])return b(o[79],c[1],a[1]);break;case
1:if(1===a[0]){var
n=a[2],p=c[2],f=a2(c[1],a[1]);return f?a2(p,n):f}break;case
2:if(2===a[0])return b(l[1][1],c[1],a[1]);break;case
3:if(3===a[0]){var
q=a[2],r=c[2],g=aK(c[1],a[1]);if(g){var
c=r,a=q;continue}return g}break;default:if(4===a[0]){var
d=a[1],e=c[1],h=e[2]===d[2]?1:0,s=d[3],t=d[1][1],u=e[3],v=e[1][1];if(h){var
i=u===s?1:0;if(i)return b(l[46],v,t);var
j=i}else
var
j=h;return j}}return 0}}function
aL(c){switch(c[0]){case
0:var
e=a(o[92],c[1]);return b($[2][1],1,e);case
1:var
f=c[1],g=a(a3[6],c[2]),h=a(a3[6],f);return j($[2][3],2,h,g);case
2:var
i=a(l[1][3],c[1]);return b($[2][1],3,i);case
3:var
k=c[1],m=aL(c[2]),n=aL(k);return j($[2][3],4,n,m);default:var
d=c[1],p=d[3],q=d[2],r=a(l[50],d[1][1]);return z($[2][4],5,r,q,p)}}var
G=a(ae[19],[0,o[79],o[92]]),V=a(ae[19],[0,aK,aL]),aM=a(ae[19],[0,l[1][1],l[1][3]]),ce=[0,a(o[1],ag[8])],a4=[0,[1,ag[8],[0,ag[8],ag[8],0]],ag[8],n[1],0,ce];function
a5(c){var
b=a(V[1],U);return[0,U,0,aA(5,a4),a(G[1],U),0,b]}function
cf(e,d){var
f=a(G[1],U),h=a(aM[1],U),i=g[2][1],j=a(v[2],0),k=a(v[2],0),l=g[2][1],b=a(an[1],U),c=[0,a(af[1],U),b];return[0,a5(0),c,l,k,j,0,0,i,h,e,0,f,d]}function
cg(a){return a[1]}function
y(e,g){var
c=0,a=g;for(;;){var
d=r(e[3],a)[a+1][2];if(0<=d){var
c=[0,a,c],a=d;continue}var
f=function(b){r(e[3],b)[b+1][2]=a;return 0};b(i[17][14],f,c);return a}}function
K(e,b){var
d=r(e[3],b)[b+1][1];if(0===d[0])return d[1];var
f=a(c[3],ch);return j(E[3],0,ci,f)}function
aN(b,a){return r(b[3],a)[a+1][3]}function
cj(d,c,a){var
e=aN(d,c);return b(n[22],a,e)}function
ck(c,f,e){var
a=f;for(;;)try{var
g=aN(c,a),h=b(n[22],e,g);return h}catch(b){b=s(b);if(b===t){var
d=r(c[3],a)[a+1][1];if(0===d[0])throw t;var
a=d[1];continue}throw b}}function
a6(e,b){var
d=r(e[3],b)[b+1][5];if(4===d[0])return d[1];var
f=a(c[3],cl);return j(E[3],0,cm,f)}function
a7(b,a){return K(b,a)[1]}function
cn(a){return a[4]}function
co(a){return a[5]}function
cp(e,d,c){var
a=K(e,d);a[1]=a[1]+1|0;a[2]=b(g[2][4],c,a[2]);a[3]=b(g[2][4],c,a[3]);return 0}function
cq(e,d,c){var
a=K(e,d);a[1]=a[1]+1|0;a[3]=b(g[2][4],c,a[3]);return 0}var
a8=[248,cr,x.caml_fresh_oo_id(0)];function
cs(b){var
c=a(i[17][6],b[3]);return[0,b[1],b[2]+1|0,c]}function
ct(a,c,e){try{var
i=b(m[22],c,a[6]),d=i}catch(a){a=s(a);if(a!==t)throw a;var
d=g[2][1]}var
f=a[6],h=b(g[2][4],e,d);a[6]=j(m[4],c,h,f);return 0}function
aa(b,a){return r(b[3],a)[a+1][5]}function
aO(f,b){var
d=r(f[3],b)[b+1][4];if(d){var
e=d[1];return[0,e[1],e[2]]}var
g=a(c[3],cu);return j(E[3],0,cv,g)}function
a9(a,c){var
b=aO(a,c),d=b[1],e=y(a,b[2]);return[0,y(a,d),e]}function
cw(a){var
b=a[2],c=b+1|0;if(c===a[1]){var
d=((a[1]*3|0)/2|0)+1|0,e=aA(d,a4);a[1]=d;bI(i[19][10],a[3],0,e,0,b);a[3]=e}a[2]=c;return b}function
ao(a){return[0,0,g[2][1],g[2][1],0,a,m[1]]}var
cy=[0,a(l[1][6],cx)],cA=[0,a(l[1][6],cz)],cB=a(o[1],2),cC=[0,0,a(o[1],2),cB],cD=a(o[10],cC);function
a_(f,c){var
g=a(o[26],f);if(10===g[0]){var
d=g[1],h=a(ap[2],0);if(b(a$[59],d[1],h)){var
e=b(l[a1][1],d[1],0);if(c){var
i=c[2],j=a(o[17],[0,e,c[1]]);return b(ab[59],j,i)}var
k=a(ap[2],0),m=b(cE[30],k,d),n=a(ap[2],0),p=b(a$[58],e,n)[2]+1|0,q=b(ab[84],p,m)[1],r=[0,e,a(o[1],1)],s=a(o[17],r);return b(ab[68],s,q)}}return b(ab[59],f,c)}function
H(c){switch(c[0]){case
0:return a_(c[1],0);case
1:var
i=c[1],f=[0,cA,a(o[5],c[2]),cD],g=a(o[11],f),h=[0,cy,a(o[5],i),g];return a(o[11],h);case
2:return a(o[2],c[1]);case
3:var
j=c[1],e=[0,H(c[2]),0],d=j;for(;;){if(3===d[0]){var
k=d[1],e=[0,H(d[2]),e],d=k;continue}if(0===d[0])return a_(d[1],e);var
l=H(d);return b(ab[59],l,e)}default:return a(o[21],c[1][1])}}function
ba(q,p){var
f=a(e[bM][1],p);function
d(b){return ba(q,a(e[8],b))}var
c=a(o[26],f);switch(c[0]){case
6:var
r=c[2],s=c[1],t=d(c[3]),u=[0,s,d(r),t];return a(o[10],u);case
7:var
v=c[2],w=c[1],x=d(c[3]),y=[0,w,d(v),x];return a(o[11],y);case
8:var
z=c[3],A=c[2],B=c[1],C=d(c[4]),D=d(z),E=[0,B,d(A),D,C];return a(o[12],E);case
9:var
F=c[1],G=b(i[19][52],d,c[2]),H=[0,d(F),G];return a(o[13],H);case
10:var
g=c[1],I=g[2],J=a(l[17][6],g[1]),K=[0,a(l[17][2],J),I];return a(o[16],K);case
11:var
h=c[1],j=h[1],L=h[2],M=j[2],N=a(l[23][6],j[1]),O=[0,[0,a(l[23][2],N),M],L];return a(o[19],O);case
12:var
k=c[1],m=k[1],n=m[1],P=k[2],Q=m[2],R=n[2],S=a(l[23][6],n[1]),T=[0,[0,[0,a(l[23][2],S),R],Q],P];return a(o[21],T);case
16:var
U=c[2],V=c[1],W=function(b){var
c=a(l[17][6],b);return a(l[17][2],c)},X=b(l[a1][10],W,V),Y=[0,X,d(U)];return a(o[17],Y);default:return f}}function
aP(b,a){if(0===a[0]){var
d=a[2],e=a[1],f=function(c,a){return[3,a,aP(b,c)]};return j(i[17][19],f,d,e)}var
c=a[1]-1|0;return r(b,c)[c+1]}function
L(f,d){var
g=a(c[3],cG),h=H(aa(f,d)),i=a(e[8],h),j=a(C[aD],i),k=a(c[3],cH),l=a(c[16],d),m=a(c[3],cI),n=b(c[12],m,l),o=b(c[12],n,k),p=b(c[12],o,j);return b(c[12],p,g)}function
aq(d){var
f=a(c[3],cJ),g=H(d),h=a(e[8],g),i=a(C[aD],h),j=a(c[3],cK),k=b(c[12],j,i);return b(c[12],k,f)}function
O(d,f){var
h=d[1];try{var
k=b(V[7],h[6],f);return k}catch(k){k=s(k);if(k===t){var
c=cw(h),u=H(f),w=a(e[8],u),x=b(p[15],d[13],w),i=ba(a(p[2],d[13]),x);switch(f[0]){case
2:var
C=n[1],l=[0,[0,ao(i)],-1,C,0,f];break;case
3:var
D=f[2],o=O(d,f[1]),q=O(d,D);cp(h,y(h,o),c);cq(h,y(h,q),c);d[3]=b(g[2][4],c,d[3]);var
E=n[1],l=[0,[0,ao(i)],-1,E,[0,[0,o,q]],f];break;case
4:var
F=f[1];b(v[3],[0,c,[0,[0,c,0]]],d[5]);b(v[3],[0,c,[1,[0,c,F[2],0]]],d[5]);var
I=n[1],l=[0,[0,ao(i)],-1,I,0,f];break;default:b(v[3],[0,c,[0,[0,c,0]]],d[5]);var
z=n[1],l=[0,[0,ao(i)],-1,z,0,f]}r(h[3],c)[c+1]=l;j(V[5],h[6],f,c);try{var
B=b(G[7],d[12],i),m=B}catch(a){a=s(a);if(a!==t)throw a;var
m=g[2][1]}var
A=b(g[2][4],c,m);j(G[10],d[12],i,A);return c}throw k}}function
bb(a,e,d,c){var
f=O(a,d),g=O(a,c);b(v[3],[0,f,g,[0,e,0]],a[4]);return j(G[5],a[1][4],e,[0,d,c])}function
bc(a,d,c,b){var
e=O(a,c),f=O(a,b);a[6]=[0,[0,e,f,d],a[6]];return 0}function
cL(b,d,c,a){b[7]=[0,[0,d,c,a[1],a[3],a[2],a[5],a[4]],b[7]];return 0}function
cM(a,d,c){try{var
e=a[1],f=function(a){return y(e,a)},g=b(i[19][15],f,c),h=b(aM[9],a[9],d),k=function(b){function
c(c,b){return c===y(a[1],b)?1:0}return j(i[19][32],c,g,b)},l=b(i[17][26],k,h);return l}catch(a){a=s(a);if(a===t)return 0;throw a}}function
cY(e,b,a,d){var
c=r(e[3],b)[b+1];c[1]=[1,a,d];c[2]=a;return 0}function
bd(g,f,e){var
a=f,b=e;for(;;){var
c=r(g[3],a)[a+1][1];if(0===c[0])return b;var
d=c[1],h=[0,[0,[0,a,d],c[2]],b],a=d,b=h;continue}}function
cZ(c,i,h){var
o=y(c,h);if(y(c,i)===o){var
p=bd(c,h,0),a=[0,bd(c,i,0),p];for(;;){var
b=a[1];if(b){var
d=a[2];if(d){var
f=d[1][1],g=b[1][1],e=g[1]===f[1]?1:0,m=d[2],n=b[2],j=f[2],k=g[2],l=e?k===j?1:0:e;if(l){var
a=[0,n,m];continue}return a}return[0,b,0]}return[0,0,a[2]]}}throw[0,be,c0]}function
bf(d,h,k,w){A(function(o){var
e=a(c[3],c1),f=L(d[1],k),g=a(c[3],c2),i=L(d[1],h),j=a(c[3],c3),l=b(c[12],j,i),m=b(c[12],l,g),n=b(c[12],m,f);return b(c[12],n,e)});var
i=K(d[1],h),e=K(d[1],k);cY(d[1],h,k,w);try{var
H=b(G[7],d[12],i[5]),p=H}catch(a){a=s(a);if(a!==t)throw a;var
p=g[2][1]}var
x=b(g[2][6],h,p);j(G[10],d[12],i[5],x);var
q=b(g[2][7],i[3],e[3]);e[1]=a(g[2][20],q);e[3]=q;e[2]=b(g[2][7],i[2],e[2]);cb(d[2],i[3]);d[3]=b(g[2][7],d[3],i[3]);var
y=r(d[1][3],h)[h+1][3];function
z(c,a){return b(v[3],[0,a,[1,c]],d[5])}b(n[10],z,y);var
B=i[6];function
C(c){function
e(a){return b(v[3],[0,a,[0,c]],d[5])}return a(g[2][13],e)}b(m[10],C,B);var
l=i[4],f=e[4];if(typeof
l==="number"){if(0===l)return 0;if(typeof
f==="number"){if(0===f){e[4]=1;return 0}}else
if(0===f[0]){d[8]=b(g[2][6],k,d[8]);e[4]=1;return 0}}else
if(0===l[0]){var
D=l[1];if(typeof
f==="number"){if(0===f){e[4]=[0,D];d[8]=b(g[2][6],h,d[8]);d[8]=b(g[2][4],k,d[8]);return 0}var
u=0}else
var
u=1===f[0]?1:0;if(!u){d[8]=b(g[2][6],h,d[8]);return 0}}else{var
o=l[1],E=o[2],F=o[1];if(typeof
f==="number"){if(0===f){e[4]=[1,o];return 0}}else
if(0!==f[0])return b(v[3],[0,F,[1,E]],d[5])}return 0}function
c4(e,f){A(function(n){var
d=a(c[3],c5),g=L(f[1],e[2]),h=a(c[3],c6),i=L(f[1],e[1]),j=a(c[3],c7),k=b(c[12],j,i),l=b(c[12],k,h),m=b(c[12],l,g);return b(c[12],m,d)});var
g=f[1],h=y(g,e[1]),i=y(g,e[2]),j=1-(h===i?1:0);if(j){var
l=a7(g,i);if(a7(g,h)<l)return bf(f,h,i,e);var
d=e[3],k=typeof
d==="number"?0:0===d[0]?[0,d[1],1-d[2]]:[1,d[3],d[4],d[1],d[2],d[5]];return bf(f,i,h,[0,e[2],e[1],k])}return j}function
da(f,s,d){A(function(j){var
e=a(c[3],db),g=L(d[1],f),h=a(c[3],dc),i=b(c[12],h,g);return b(c[12],i,e)});var
p=y(d[1],f),h=K(d[1],p);if(0===s[0]){ct(h,s[1],f);d[3]=b(g[2][7],h[2],d[3]);return 0}var
e=s[1],q=r(d[1][3],p)[p+1];if(1-b(n[3],e,q[3]))q[3]=j(n[4],e,f,q[3]);var
i=h[4];if(typeof
i==="number"){if(0===i)return 0===e[2]?(h[4]=[1,[0,f,e]],0):(d[3]=b(g[2][7],h[2],d[3]),h[4]=[0,e],d[8]=b(g[2][4],p,d[8]),0)}else
if(1===i[0]){var
t=i[1],k=t[2],u=t[1];if(e[1]===k[1]){var
x=a6(d[1],e[1]),o=x[3],m=k[3],l=e[3];for(;;){var
w=0<o?1:0;if(w){if(m)if(l){var
z=l[2],B=m[2];b(v[3],[0,m[1],l[1],[1,u,k,f,e,o]],d[4]);var
o=o-1|0,m=B,l=z;continue}var
C=a(c[3],c_);return j(E[3],0,c$,C)}return w}}throw[0,a8,u,k,f,e]}d[3]=b(g[2][7],h[2],d[3]);return 0}function
dd(e){var
g=e[1];function
h(f){if(f){var
d=f[1],k=f[2],l=y(g,d[2]);if(y(g,d[1])===l)var
j=a(c[3],de),i=[0,d];else
var
m=h(k),j=a(c[3],di),i=m;A(function(p){var
f=a(c[3],df),g=L(e[1],d[2]),h=a(c[3],dg),i=L(e[1],d[1]),k=a(c[3],dh),l=b(c[12],k,i),m=b(c[12],l,h),n=b(c[12],m,g),o=b(c[12],n,f);return b(c[12],o,j)});return i}return 0}return h(e[6])}var
dk=a(l[1][6],dj);function
dp(d){var
f=d[8];function
h(r){var
h=K(d[1],r)[4];if(typeof
h!=="number"&&0===h[0]){var
f=h[1],w=H(aa(d[1],f[1])),x=a(e[8],w),y=b(p[15],d[13],x),z=a(e[bM][1],y),A=f[3],B=function(a){return H(aa(d[1],a))},C=b(i[17][15],B,A),D=a(i[17][9],C),F=b(ab[75],z,D),G=f[2],l=aa(d[1],r),n=F,k=G;for(;;){if(0<k){var
q=a(o[60],n),t=q[3],u=q[2],g=b(p[20],dk,d[13]),m=d[13];d[13]=j(dl[3][11],m[2],m[1],[0,[0,g,u],0]);var
v=[0,a(o[2],g),0],l=[3,l,[2,g]],n=b(dn[13],v,t),k=k-1|0;continue}d[1][5]=[0,f,d[1][5]];O(d,l);return 0}}var
s=a(c[3],dm);return j(E[3],0,0,s)}return b(g[2][13],h,f)}function
bg(c){var
a=[0,m[1]],f=c[1],d=c[1][3];function
e(c,h){var
d=c<f[2]?1:0;if(d){var
e=h[1];if(0===e[0]){var
i=e[1][6],k=function(d,k){try{var
i=b(m[22],d,a[1]),e=i}catch(a){a=s(a);if(a!==t)throw a;var
e=g[2][1]}var
f=a[1],h=b(g[2][4],c,e);a[1]=j(m[4],d,h,f);return 0};return b(m[10],k,i)}return 0}return d}b(i[19][14],e,d);return a[1]}function
bh(q,p,d){var
c=a(i[22][9],d),l=c[3];if(l){var
e=l[2],u=l[1],f=u[2],h=u[1],j=q[1];if(0===h[0]){var
k=h[2],n=h[1];if(k){var
A=k[2],B=k[1];try{var
C=b(V[7],j[6],n),D=[0,C,a(i[17][1],k)],E=K(j,f)[6],F=b(m[22],D,E),G=function(g){var
f=a9(q[1],g),h=[0,[0,[0,n,A],f[1]],[0,[0,B,f[2]],e]],j=c[2],k=[0,a(i[19][8],c[1]),j,h];return b(i[22][3],k,d)},H=b(g[2][13],G,F);return H}catch(a){a=s(a);if(a===t)return 0;throw a}}try{var
v=y(j,b(V[7],j[6],n))===f?1:0,I=v?b(i[22][3],[0,c[1],c[2],e],d):v;return I}catch(a){a=s(a);if(a===t)return 0;throw a}}var
o=h[1],w=o-1|0;if(0<=r(c[1],w)[w+1]){var
x=o-1|0;return r(c[1],x)[x+1]===f?b(i[22][3],[0,c[1],c[2],e],d):0}var
z=o-1|0;r(c[1],z)[z+1]=f;return b(i[22][3],[0,c[1],c[2],e],d)}p[1]=[0,[0,c[2],c[1]],p[1]];return 0}function
aR(d,c){if(0===c[0]){var
e=c[1],f=a(i[17][1],c[2]);return[0,b(V[7],d,e),f]}return b(E[9],0,dq)}function
bi(c){var
k=c[1][6],f=a(i[22][2],0),l=bg(c);function
d(a){var
h=a[5];if(typeof
h==="number")if(0===h)try{var
x=aR(k,a[4]),y=b(m[22],x,l),d=y}catch(a){a=s(a);if(a!==t)throw a;var
d=g[2][1]}else
var
d=g[2][1];else{var
z=h[1];try{var
A=b(G[7],c[12],z),o=A}catch(a){a=s(a);if(a!==t)throw a;var
o=g[2][1]}var
d=o}function
p(c){return b(i[22][3],[0,aA(a[3],-1),a,[0,[0,a[4],c],0]],f)}b(g[2][13],p,d);var
j=a[7];if(typeof
j==="number")if(0===j)try{var
r=aR(k,a[6]),u=b(m[22],r,l),e=u}catch(a){a=s(a);if(a!==t)throw a;var
e=g[2][1]}else
var
e=g[2][1];else{var
v=j[1];try{var
w=b(G[7],c[12],v),n=w}catch(a){a=s(a);if(a!==t)throw a;var
n=g[2][1]}var
e=n}function
q(c){return b(i[22][3],[0,aA(a[3],-1),a,[0,[0,a[6],c],0]],f)}return b(g[2][13],q,e)}b(i[17][14],d,c[7]);return f}function
bj(b){var
d=[0,0],e=bi(b);A(function(b){return a(c[3],dr)});try{for(;;){a(aQ[3],0);bh(b,d,e);continue}}catch(a){a=s(a);if(a===i[22][1])return d[1];throw a}}function
aS(w,d){A(function(b){return a(c[3],ds)});try{for(;;){a(aQ[3],0);try{c4(a(v[5],d[4]),d);var
M=1,h=M}catch(e){e=s(e);if(e!==v[1])throw e;try{var
u=a(v[5],d[5]);da(u[1],u[2],d);var
J=1,h=J}catch(e){e=s(e);if(e!==v[1])throw e;try{var
f=a(g[2][26],d[3]);d[3]=b(g[2][6],f,d[3]);A(function(i){return function(j){var
e=a(c[3],c8),f=L(d[1],i),g=a(c[3],c9),h=b(c[12],g,f);return b(c[12],h,e)}}(f));var
l=a9(d[1],f),p=l[1],y=aO(d[1],f)[2],q=K(d[1],p),r=q[4],X=typeof
r==="number"?0:0===r[0]?(q[4]=1,d[8]=b(g[2][6],p,d[8]),1):0,z=aN(d[1],p),B=function(c,e){return function(a,f){return b(v[3],[0,e,[1,[0,a[1],a[2]-1|0,[0,c,a[3]]]]],d[5])}}(y,f);b(n[10],B,z);var
D=q[6],F=function(c){return function(a,e){return b(v[3],[0,c,[0,[0,a[1],a[2]+1|0]]],d[5])}}(f);b(m[10],F,D);try{var
G=b$(l,d[2]);b(v[3],[0,f,G,0],d[4])}catch(a){a=s(a);if(a!==t)throw a;b8(f,l,d[2])}var
I=1,h=I}catch(a){a=s(a);if(a!==t)throw a;var
h=0}}}if(h)continue;var
x=dd(d);if(x)var
S=x[1],T=w?[1,S]:0,k=[0,T];else
if(a(g[2][2],d[8]))if(0<d[10]){var
U=bj(d),V=function(q){var
m=q[2],f=q[1];a(aQ[3],0);var
n=0<d[10]?1:0;if(n){if(cM(d,f[1],m))return A(function(b){return a(c[3],cN)});j(aM[5],d[9],f[1],m);var
t=d[1],r=function(b){try{var
e=aa(t,b);return e}catch(b){b=s(b);if(a(E[20],b)){var
d=a(c[3],cF);return j(E[3],0,0,d)}throw b}},l=b(i[19][15],r,m),u=a(o[2],f[1]),p=b(i[19][15],H,l);a(i[19][41],p);var
g=a(o[13],[0,u,p]),h=aP(l,f[4]),k=aP(l,f[6]);d[11]=1;d[10]=d[10]-1|0;return f[2]?(A(function(B){var
f=a(c[3],cO),i=aq(k),j=a(c[3],cP),l=aq(h),m=a(c[3],cQ),n=a(e[8],g),o=a(C[aD],n),p=a(c[3],cR),q=b(c[12],p,o),r=b(c[12],q,m),s=b(c[12],r,l),t=b(c[12],s,j),u=b(c[12],t,i),v=b(c[12],u,f),w=a(c[5],0),x=a(c[16],d[10]),y=a(c[3],cS),z=b(c[12],y,x),A=b(c[12],z,w);return b(c[12],A,v)}),bb(d,g,h,k)):(A(function(B){var
f=a(c[3],cT),i=aq(k),j=a(c[3],cU),l=aq(h),m=a(c[3],cV),n=a(e[8],g),o=a(C[aD],n),p=a(c[3],cW),q=b(c[12],p,o),r=b(c[12],q,m),s=b(c[12],r,l),t=b(c[12],s,j),u=b(c[12],t,i),v=b(c[12],u,f),w=a(c[5],0),x=a(c[16],d[10]),y=a(c[3],cX),z=b(c[12],y,x),A=b(c[12],z,w);return b(c[12],A,v)}),bc(d,[0,g],h,k))}return n};b(i[17][14],V,U);var
W=d[11]?(d[11]=0,aS(1,d)):(A(function(b){return a(c[3],dt)}),0),k=W}else{A(function(b){return a(c[3],du)});var
k=0}else{A(function(b){return a(c[3],dv)});dp(d);var
k=aS(0,d)}return k}}catch(a){a=s(a);if(a[1]===a8){var
N=a[5],O=a[4],P=a[3],Q=a[2],R=w?[0,[0,Q,P,O,N]]:0;return[0,R]}throw a}}var
f=[0,[0,m[1],m[2],m[3],m[4],m[5],m[6],m[7],m[8],m[9],m[10],m[11],m[12],m[13],m[14],m[15],m[16],m[17],m[18],m[19],m[20],m[21],m[22],m[23],m[24]],[0,n[1],n[2],n[3],n[4],n[5],n[6],n[7],n[8],n[9],n[10],n[11],n[12],n[13],n[14],n[15],n[16],n[17],n[18],n[19],n[20],n[21],n[22],n[23],n[24]],G,V,aK,H,A,cg,cn,co,cf,O,bb,bc,cL,cs,y,cj,ck,aa,a6,aO,cZ,bg,bh,bi,aR,bj,aS,L,a5];al(161,f,"Cc_plugin.Ccalgo");function
ah(a){return[0,a,a,[2,a]]}function
ac(b,a){var
c=b[3],d=a[3];if(2===c[0])if(2===d[0])return ah([3,c[1],d[1]]);return[0,[3,b[1],a[1]],[3,b[2],a[2]],[4,b,a]]}function
B(o,n){var
e=o,d=n;for(;;){var
g=e[3],h=d[3];switch(g[0]){case
2:return d;case
4:var
l=g[2],m=g[1];switch(h[0]){case
2:var
i=0;break;case
3:var
k=h[1][3];if(4===k[0]){var
r=h[2],s=k[1],t=B(l,k[2]),e=ac(B(m,s),t),d=r;continue}var
i=1;break;case
4:var
u=h[1],v=B(l,h[2]);return ac(B(m,u),v);default:var
i=1}break;default:var
i=0}if(!i){if(2===h[0])return e;if(3===g[0]){var
q=g[1],e=q,d=B(g[2],d);continue}}if(b(f[5],e[2],d[1]))return[0,e[1],d[2],[3,e,d]];var
p=a(c[3],dw);return j(E[3],0,0,p)}}function
M(b){var
a=b[3];switch(a[0]){case
0:return[0,b[2],b[1],[1,a[1]]];case
1:return[0,b[2],b[1],[0,a[1]]];case
2:return b;case
3:var
c=a[2],d=M(a[1]);return B(M(c),d);case
4:var
e=a[1],f=M(a[2]);return ac(M(e),f);default:var
g=a[4],h=a[3],i=a[2],j=[5,M(a[1]),i,h,g];return[0,b[2],b[1],j]}}function
bk(d,a){var
c=b(f[3][7],d,a);return[0,c[1],c[2],[0,a]]}function
bl(d,a){var
c=b(f[3][7],d,a);return[0,c[2],c[1],[1,a]]}function
bm(f,e){var
b=f,d=e;for(;;){if(3===b[0]){var
h=b[2],i=b[1];if(0<d){var
b=i,d=d-1|0;continue}return h}var
g=a(c[3],dx);return j(E[3],0,dy,g)}}function
bn(c,d,b,a){var
e=bm(c[2],b-a|0);return[0,bm(c[1],b-a|0),e,[5,c,d,b,a]]}function
P(d,e,g){function
i(n){var
h=b(f[30],d,g),i=a(c[4],dz),j=b(f[30],d,e),k=a(c[3],dA),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],i);if(e===g)return ah(b(f[20],d,e));var
h=j(f[23],d,e,g),k=h[1],l=M(ar(d,g,h[2]));return B(ar(d,e,k),l)}function
bo(d,i){var
g=i[2],j=i[1],k=j[2],l=j[1];function
p(n){var
e=b(f[30],d,k),g=a(c[4],dB),h=b(f[30],d,l),i=a(c[3],dC),j=b(c[12],i,h),m=b(c[12],j,g);return b(c[12],m,e)}a(f[7],p);var
q=P(d,l,g[1]),r=M(P(d,k,g[2])),e=g[3];if(typeof
e==="number")var
h=bp(d,g[1],g[2]);else
if(0===e[0])var
m=e[1],s=e[2]?bl(a(f[9],d),m):bk(a(f[9],d),m),h=s;else
var
n=e[2],t=e[5],u=aU(d,e[1],n,e[3],e[4]),o=b(f[21],d,n[1]),h=bn(u,o[1],o[3],t);return B(B(q,h),r)}function
aT(d,g,e){function
l(k){var
e=a(c[4],dD),h=b(f[30],d,g),i=a(c[3],dE),j=b(c[12],i,h);return b(c[12],j,e)}a(f[7],l);var
h=j(f[19],d,g,e),i=P(d,g,h);if(0===e[3])return i;var
m=a(f[16],e),k=b(f[22],d,h),n=k[1],o=b(f[20],d,k[2]),p=aT(d,n,m);return B(i,ac(p,ah(o)))}function
ar(e,g,d){function
i(u){var
h=a(c[3],dF);function
i(b){return a(c[16],b[1][2])}function
k(b){return a(c[3],dG)}var
l=j(c[39],k,i,d),m=a(c[3],dH),n=a(c[4],dI),o=b(f[30],e,g),p=a(c[3],dJ),q=b(c[12],p,o),r=b(c[12],q,n),s=b(c[12],r,m),t=b(c[12],s,l);return b(c[12],t,h)}a(f[7],i);if(d){var
h=d[1],k=d[2],l=bo(e,h);return B(ar(e,h[1][2],k),l)}return ah(b(f[20],e,g))}function
bp(d,g,e){function
j(n){var
h=b(f[30],d,e),i=a(c[4],dK),j=b(f[30],d,g),k=a(c[3],dL),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
h=b(f[22],d,g),k=h[2],l=h[1],i=b(f[22],d,e),m=i[1],n=P(d,k,i[2]);return ac(P(d,l,m),n)}function
aU(d,g,i,e,h){function
j(n){var
h=b(f[30],d,e),i=a(c[4],dM),j=b(f[30],d,g),k=a(c[3],dN),l=b(c[12],k,j),m=b(c[12],l,i);return b(c[12],m,h)}a(f[7],j);var
k=P(d,g,e),l=aT(d,g,i),m=B(k,aT(d,e,h));return B(M(l),m)}var
as=[0,ah,ac,B,M,bk,bl,bn,P,bo,ar,bp,aU,function(c,b){if(bJ<=b[1]){var
a=b[2];return aU(c,a[1],a[2],a[3],a[4])}var
d=b[2];return P(c,d[1],d[2])}];al(162,as,"Cc_plugin.Ccproof");function
W(b,a){return[N,function(c){return j(aV[2],dO,b,a)}]}var
aW=W(dQ,dP),dT=W(dS,dR),bq=W(dV,dU),dY=W(dX,dW),br=W(d0,dZ),w=W(d2,d1),Q=W(d4,d3);function
bs(c,b,a){return z(bu[16],bt[14],c,b,a)}function
at(c,b,a){return z(bu[16],bt[9],c,b,a)}function
au(c,b,a){return j(ad[4],c,[0,b],a)}function
D(f,c,z){var
g=z;for(;;){var
A=bs(f,c,g),d=b(e[3],c,A);switch(d[0]){case
6:var
m=d[3],n=d[2];if(j(e[aG][13],c,1,m)){var
p=a(C[47],m),B=au(f,c,p),E=au(f,c,n),F=D(f,c,p);return[3,[3,[1,E,B],D(f,c,n)],F]}break;case
9:var
G=d[2],H=D(f,c,d[1]),I=function(a){return D(f,c,a)},J=b(i[19][15],I,G),K=function(b,a){return[3,b,a]};return j(i[19][17],K,H,J);case
10:var
q=d[1],L=q[1],M=b(e[2][2],c,q[2]),N=a(l[17][6],L),O=[0,a(l[17][2],N),M];return[0,a(o[16],O)];case
11:var
r=d[1],s=r[1],P=s[2],Q=s[1],R=b(e[2][2],c,r[2]),S=a(l[23][6],Q),T=[0,[0,a(l[23][2],S),P],R];return[0,a(o[19],T)];case
12:var
u=d[1],v=u[1],w=v[2],x=v[1],U=x[2],V=x[1],W=b(e[2][2],c,u[2]),X=a(l[23][6],V),h=[0,a(l[23][2],X),U],Y=a(ap[27],h)[1],y=b(d5[44],f,[0,h,w]);return[4,[0,[0,[0,h,w],W],y,y-Y[6]|0]];case
16:var
Z=d[2],_=d[1],$=function(b){var
c=a(l[17][6],b);return a(l[17][2],c)},aa=b(l[a1][10],$,_),g=bI(d6[9],f,c,aa,Z,0);continue}var
k=b(C[60],c,g);if(b(e[aG][16],c,k))return[0,b(e[5],c,k)];throw t}}function
aX(d,c,g){var
k=at(d,c,g),h=b(e[3],c,k);if(9===h[0]){var
f=h[2],i=S(w),l=h[1],m=T===i?w[1]:N===i?a(R[2],w):w;if(j(C[am],c,m,l))if(3===f.length-1){var
n=D(d,c,r(f,2)[3]),o=D(d,c,r(f,1)[2]);return[0,aC,[0,r(f,0)[1],o,n]]}return[0,a0,D(d,c,g)]}return[0,a0,D(d,c,g)]}function
ai(d,c,h){var
r=bs(d,c,h),f=b(e[3],c,r);switch(f[0]){case
0:var
k=f[1];return[0,[1,k],a(g[2][5],k)];case
6:var
l=f[3],m=f[2];if(j(e[aG][13],c,1,l)){var
n=a(C[47],l),o=ai(d,c,m),t=o[2],u=o[1],p=ai(d,c,n),v=p[2],w=p[1],x=au(d,c,n),y=au(d,c,m);return[0,[0,[1,y,x],[0,u,[0,w,0]]],b(g[2][7],t,v)]}break;case
9:var
z=f[2],A=D(d,c,f[1]),B=function(a){return ai(d,c,a)},E=b(i[19][49],B,z),q=a(i[17][44],E),F=q[1],G=j(i[17][18],g[2][7],g[2][1],q[2]);return[0,[0,A,a(i[17][9],F)],G]}var
s=D(d,c,h);return[0,[0,s,0],g[2][1]]}function
bv(a){return 0===a[0]?1:0}function
bw(k,c,i,v){try{var
x=at(k,c,v),l=b(e[73],c,x)}catch(a){a=s(a);if(a===o[54])throw t;throw a}var
d=l[2],m=S(w),y=l[1],z=T===m?w[1]:N===m?a(R[2],w):w;if(j(C[am],c,z,y))if(3===d.length-1){var
n=ai(k,c,r(d,1)[2]),p=n[1],A=n[2],q=ai(k,c,r(d,2)[3]),u=q[1],B=q[2];if(a(g[2][20],A)===i)if(bv(p))var
f=0;else
var
E=r(d,0)[1],f=[0,b(e[5],c,E)];else
var
f=1;if(a(g[2][20],B)===i)if(bv(u))var
h=0;else
var
D=r(d,0)[1],h=[0,b(e[5],c,D)];else
var
h=1;if(1===f)if(1===h)throw t;return[0,i,f,p,h,u]}throw t}function
d7(o,c,n,m){var
d=o,f=n,h=m;for(;;){var
p=at(d,c,h),g=b(e[3],c,p);if(6===g[0]){var
i=g[3],k=g[2],l=S(Q),q=g[1],r=T===l?Q[1]:N===l?a(R[2],Q):Q;if(j(C[am],c,r,i))return[0,bR,bw(d,c,f,k)];var
d=b(e[bX],[0,q,k],d),f=f+1|0,h=i;continue}return[0,bK,bw(d,c,f,h)]}}function
d8(d,c,g){var
n=at(d,c,g),f=b(e[3],c,n);if(6===f[0]){var
k=f[3],l=f[2],m=S(Q),o=f[1],p=T===m?Q[1]:N===m?a(R[2],Q):Q;if(j(C[am],c,p,k)){var
h=aX(d,c,l);if(aC<=h[1]){var
i=h[2];return[0,bN,[0,i[1],i[2],i[3]]]}return[0,bW,h[2]]}try{var
q=d7(b(e[bX],[0,o,l],d),c,1,k);return q}catch(a){a=s(a);if(a===t)return[0,a0,D(d,c,g)];throw a}}return aX(d,c,g)}function
X(c,g,f){var
d=S(c);function
h(b){return a(f,a(e[21],[0,b,g]))}var
i=T===d?c[1]:N===d?a(R[2],c):c,j=a(q[66][59],i);return b(k[71][1],j,h)}function
aj(c,n,w){function
d(h){var
d=S(c);function
f(o){var
q=a(k[66][5],h),x=a(k[66][3],h);function
c(r){var
y=b(p[42][8],h,o),A=a(i[19][11],n),B=j(C[58],r,y,A),c=r,k=B,f=w,d=0,D=a(e[21],[0,o,n]);for(;;){if(0===f){var
E=[0,D,a(i[17][9],d)],s=a(e[34],E),t=[0,c];z(ad[5],q,t,s,x);return[0,t[1],s]}var
g=b(e[3],c,k);if(6===g[0]){var
u=g[3],l=ff(d$[4],q,c,0,0,0,0,0,0,g[2]),m=l[2],v=l[1],c=v,k=b(e[aG][5],m,u),f=f-1|0,d=[0,m,d];continue}throw[0,be,d_]}}return b(ea[2],0,c)}var
g=T===d?c[1]:N===d?a(R[2],c):c,l=a(q[66][59],g);return b(k[71][1],l,f)}return a(k[66][10],d)}function
av(d,c){function
e(e){var
f=ad[2];function
g(a){return b(f,0,a)}var
h=j(p[42][1],g,e,c)[1],i=b(F[136],d,c),l=a(k[64][1],h);return b(k[18],l,i)}return a(k[66][10],e)}function
bz(c,b,a){return aB(bA[5],[0,ed[112]],0,ec,eb,c,b,a)}function
I(f,e){function
c(c){var
g=a(k[66][5],c),d=bz(g,a(p[42][4],c),f),h=d[1],i=a(e,d[2]),j=a(k[64][1],h);return b(k[18],j,i)}return a(k[66][10],c)}function
u(b){var
c=a(f[6],b);return a(e[8],c)}function
J(j){function
d(i){function
f(a){return b(p[42][7],i,a)}try{var
d=j[3];switch(d[0]){case
0:var
D=a(e[8],d[1]),g=a(F[45],D);break;case
1:var
E=a(e[8],d[1]),w=u(j[1]),G=u(j[2]),H=function(a){return X(dY,[0,a,G,w,E],F[45])},g=I(f(w),H);break;case
2:var
x=d[1],K=u(x),L=function(a){var
b=F[45];return X(bq,[0,a,u(x)],b)},g=I(f(K),L);break;case
3:var
y=d[2],o=d[1],M=u(o[1]),z=u(o[2]),N=u(y[2]),O=function(a){var
c=aj(br,[0,a,M,z,N],2),d=[0,J(y),0],e=[0,J(o),d];return b(q[66][19],c,e)},g=I(f(z),O);break;case
4:var
r=d[2],t=d[1],m=u(t[1]),h=u(r[1]),n=u(t[2]),A=u(r[2]),P=function(g){function
d(j){function
d(d){var
f=a(l[1][6],ee),k=b(p[42][11],f,i),o=[0,a(e[9],1),[0,h]],s=[0,[0,k],g,a(e[21],o)],u=aj(aW,[0,g,d,a(e[19],s),m,n],1),v=aj(aW,[0,j,d,n,h,A],1),w=a(e[21],[0,n,[0,A]]),x=a(e[21],[0,n,[0,h]]),y=aj(br,[0,d,a(e[21],[0,m,[0,h]]),x,w],2),z=a(c[3],ef),B=[0,b(q[66][5],0,z),0],C=[0,F[122],B],D=J(r),E=[0,b(q[66][3],v,D),C],G=[0,a(q[66][24],E),0],H=J(t),I=[0,b(q[66][3],u,H),G];return b(q[66][19],y,I)}return I(f(a(e[21],[0,m,[0,h]])),d)}return I(f(h),d)},g=I(f(m),P);break;default:var
v=d[1],Q=d[4],R=d[3],S=d[2],B=u(v[1]),T=u(v[2]),C=u(j[1]),U=a(e[9],(1+R|0)-Q|0),V=function(c){function
d(o){var
f=S[1][2],d=a(p[42][4],i),g=a(e[9],1),h=a(p[42][5],i),j=aB(by[38],h,d,f,g,c,U,C),m=a(l[1][6],d9),n=[0,[0,b(p[42][11],m,i)],c,j],r=aj(aW,[0,c,o,a(e[19],n),B,T],1),s=J(v),t=b(q[66][3],r,s),u=a(k[64][1],d);return b(q[66][3],u,t)}return I(f(C),d)},g=I(f(B),V)}return g}catch(c){c=s(c);if(a(k[70][10],c))return b(k[21],0,c);throw c}}return a(k[66][10],d)}function
eh(c){function
d(d){var
e=ad[2];function
f(a){return b(e,0,a)}var
g=j(p[42][1],f,d,c)[1],h=a(F[45],c),i=a(k[64][1],g);return b(k[18],i,h)}return a(k[66][10],d)}function
bB(j,h,f,i){function
c(c){var
g=u(h),d=u(f);function
k(f){var
m=a(l[1][6],ei),h=b(p[42][11],m,c),n=a(l[1][6],ej),o=b(p[42][11],n,c),r=[0,[0,o],f,a(e[9],1)],s=a(e[19],r),t=[0,X(dT,[0,f,g,s,j,d,a(e[10],h)],eh),0],k=[0,f,g,d],u=[0,J(i),t],v=[0,h],x=X(w,k,function(a){return av(v,a)});return b(q[66][19],x,u)}return I(b(p[42][7],c,d),k)}return a(k[66][10],c)}function
bC(Z,Y){function
d(h){var
_=a(p[42][4],h);a(aV[3],aV[10]);function
$(b){return a(c[3],em)}a(f[7],$);var
A=a(p[42][5],h),r=a(p[42][4],h),N=[0,a(k[66][14],h),r],d=b(f[11],Z,N),s=[0,0],B=[0,0];function
O(a){var
c=D(A,r,a);b(f[12],d,c);return 0}b(i[17][14],O,Y);var
P=a(k[66][4],h);function
Q(j){var
h=a(bx[2][1][1],j),e=a(o[2],h),c=d8(A,r,a(bx[2][1][3],j)),g=c[1];if(aC<=g){if(bR<=g)return bK<=g?z(f[15],d,h,1,c[2]):z(f[15],d,h,0,c[2]);if(bN<=g){var
k=c[2];return z(f[14],d,[0,e],k[2],k[3])}var
l=c[2];return z(f[13],d,e,l[2],l[3])}if(bW<=g){var
m=c[2],p=s[1],q=function(a){return z(f[14],d,[2,a[1],e],a[2],m)};b(i[17][14],q,p);B[1]=[0,[0,e,m],B[1]];return 0}var
n=c[2],t=B[1];function
u(a){return z(f[14],d,[2,e,a[1]],n,a[2])}b(i[17][14],u,t);s[1]=[0,[0,e,n],s[1]];return 0}b(i[17][14],Q,P);var
C=aX(A,r,a(p[42][6],h));if(aC<=C[1]){var
H=C[2];z(f[14],d,0,H[2],H[3])}else{var
R=C[2],S=s[1],T=function(a){return z(f[14],d,[1,a[1]],a[2],R)};b(i[17][14],T,S)}function
aa(b){return a(c[3],en)}a(f[7],aa);var
K=b(f[29],1,d);function
ab(b){return a(c[3],eo)}a(f[7],ab);var
g=a(f[8],d);if(K){var
t=K[1],ac=function(b){return a(c[3],ep)};a(f[7],ac);if(typeof
t==="number"){var
L=a(k[66][5],h),ad=a(f[10],g),ae=function(c){var
h=b(f[21],g,c[1]),j=c[3];function
k(a){return u(b(f[20],g,a))}var
l=b(i[17][17],k,j),d=h[1],m=d[1],n=c[2],o=[0,m,a(e[2][1],d[2])],p=[0,a(e[28],o),l];return[0,a(e[34],p),n]},af=b(i[17][15],ae,ad),ag=b(bD[3],0,eq),ah=function(a){var
c=a[2],d=aB(es[9],0,er,0,l[1][10][1],L,_,a[1]);function
e(a){return ag}var
f=[4,d,b(i[17][54],c,e)],g=b(bD[3],0,f);return b(et[42],L,g)},ai=a(c[3],eu);b(aJ[6],0,ai);var
aj=a(c[3],ev),ak=a(c[3],ew),al=function(h){var
d=a(c[3],ex),e=a(c[13],0),f=a(c[3],ey),g=b(c[12],f,e);return b(c[12],g,d)},am=j(c[39],al,ah,af),an=a(c[3],ez),ao=b(c[12],an,am),ap=b(c[12],ao,ak),aq=b(c[26],8,ap),ar=a(c[3],eA),at=b(c[12],ar,aq),au=b(c[12],at,aj);b(aJ[6],0,au);var
aw=a(c[3],eB);return b(q[66][4],0,aw)}else{if(0===t[0]){var
v=t[1],M=v[2],E=b(as[13],g,[0,bJ,[0,v[1],M,v[3],v[4]]]);b(f[21],g,M[1]);var
W=function(c){var
d=u(E[1]),g=u(E[2]),h=a(k[66][5],c),i=a(p[42][4],c),e=bz(h,i,b(p[42][7],c,d)),j=e[2],m=e[1],n=a(l[1][6],el),f=b(p[42][11],n,c),r=[0,a(by[16],f),0],o=[0,j,d,g],s=[0,J(E),r],t=[0,f],y=X(w,o,function(a){return av(t,a)}),v=b(q[66][19],y,s),x=a(k[64][1],m);return b(q[66][3],x,v)};return a(k[66][10],W)}var
n=t[1],x=b(as[13],g,[0,-608347012,[0,n[1],n[2]]]),G=b(f[20],g,n[1]),y=b(f[20],g,n[2]),m=n[3];if(typeof
m==="number")return J(x);else
switch(m[0]){case
0:var
ax=a(e[8],m[1]),U=function(c){var
d=u(G),g=u(y),h=a(l[1][6],eg),f=b(p[42][11],h,c),i=[0,ax,[0,a(e[10],f)]],j=a(e[21],i);function
k(c){var
h=[0,a(F[99],j),0],e=[0,c,d,g],i=[0,J(x),h],k=[0,f],l=X(w,e,function(a){return av(k,a)});return b(q[66][19],l,i)}return I(b(p[42][7],c,d),k)};return a(k[66][10],U);case
1:return bB(a(e[8],m[1]),G,y,x);default:var
ay=m[2],az=a(e[8],m[1]),aA=a(e[8],ay),V=function(d){var
f=u(y),g=a(l[1][6],ek),c=b(p[42][11],g,d),h=[0,aA,[0,a(e[10],c)]],i=a(e[21],h),j=[0,a(F[99],i),0],k=[0,bB(az,G,y,x),j],m=av([0,c],f);return b(q[66][19],m,k)};return a(k[66][10],V)}}}var
aD=a(c[3],eC);return b(q[66][4],0,aD)}return a(k[66][10],d)}var
eE=a(c[3],eD),bE=b(q[66][5],0,eE);function
bF(d,c){var
e=bC(d,c),f=a(q[66][30],F[17]),g=b(q[66][3],f,e);return b(q[66][12],g,bE)}function
eF(c,d,l,i){var
f=S(c);function
g(m){function
c(c){var
n=ad[2];function
o(a){return b(n,0,a)}var
f=j(p[42][1],o,c,d),q=f[2],r=f[1],s=a(p[42][5],c),g=aB(bA[5],0,0,0,eG,s,r,q),t=g[1],h=a(e[21],[0,m,[0,g[2],d,l]]),u=a(p[42][5],c),v=z(ad[2],0,u,t,h)[1],w=a(i,h),x=a(k[64][1],v);return b(k[18],x,w)}return a(k[66][10],c)}var
h=T===f?c[1]:N===f?a(R[2],c):c,m=a(q[66][59],h);return b(k[71][1],m,g)}function
eH(l){var
y=a(k[66][3],l),c=a(p[42][4],l);function
z(d,c){try{var
e=0,f=F[86],g=[0],h=function(a){return X(bq,g,a)}(f),i=[0,a(q[66][22],h),e],j=[0,a(k[16],0),i],l=eF(w,d,c,F[143]),m=b(q[66][19],l,j);return m}catch(c){c=s(c);if(a(k[70][10],c))return b(k[21],0,c);throw c}}function
A(d){var
c=d[1],e=d[2];if(c[1]!==eI[1])if(c[1]!==eJ[1])return b(k[21],[0,e],c);return a(k[16],0)}var
g=b(e[3],c,y);if(9===g[0]){var
h=g[2];if(3===h.length-1){var
n=S(w),B=g[1],D=h[2],E=h[3],G=T===n?w[1]:N===n?a(R[2],w):w;if(j(C[am],c,G,B)){var
o=b(e[3],c,D),t=b(e[3],c,E);if(9===o[0])if(9===t[0]){var
v=t[2],i=o[2];if(i.length-1===v.length-1)var
x=function(c){if(0<=c){var
d=x(c-1|0),e=r(v,c)[c+1],f=z(r(i,c)[c+1],e);return b(q[66][16],f,d)}var
g=bF(aZ,0);return a(q[66][22],g)},u=x(i.length-1-1|0),f=1;else
var
f=0}else
var
f=0;else
var
f=0;if(!f)var
u=a(k[16],0);var
m=u,d=1}else
var
d=0}else
var
d=0}else
var
d=0;if(!d)var
m=a(k[16],0);return b(k[23],m,A)}var
Y=[0,J,bC,bE,bF,a(k[66][10],eH)];al(184,Y,"Cc_plugin.Cctac");a(eK[10],aw);var
eL=0;function
eM(c,a,d){return b(Y[4],c,a)}var
eO=a(l[1][7],eN),eP=[0,[0,[5,a(ay[16],ax[13])]],eO],eR=[0,eQ,[1,b(az[11],0,eP),0]],eT=a(l[1][7],eS),eU=[0,[5,a(ay[16],ax[21])],eT],eW=[0,[0,[0,eV,[1,b(az[11],0,eU),eR]],eM],eL];function
eX(a,c){return b(Y[4],aZ,a)}var
eZ=a(l[1][7],eY),e0=[0,[0,[5,a(ay[16],ax[13])]],eZ],e3=[0,[0,[0,e2,[0,e1,[1,b(az[11],0,e0),0]]],eX],eW];function
e4(a,c){return b(Y[4],a,0)}var
e6=a(l[1][7],e5),e7=[0,[5,a(ay[16],ax[21])],e6],e9=[0,[0,[0,e8,[1,b(az[11],0,e7),0]],e4],e3],e$=[0,[0,e_,function(a){return b(Y[4],aZ,0)}],e9];z(bG[10][8],aw,fa,0,e$);var
fb=0,fd=[0,[0,fc,function(a){return Y[5]}],fb];z(bG[10][8],aw,fe,0,fd);var
bH=[0,aw];al(190,bH,"Cc_plugin.G_congruence");al(191,[0,f,as,Y,bH],"Cc_plugin");return}
