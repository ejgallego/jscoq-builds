function(A9){"use strict";var
gG="QMicromega",kM="__varmap",ln="__p",k2="__p%i",lm="__ff",kk="=",gK=">=",ch=" * ",J="micromega",ll="enum_proof\n",e0='command "',kL="( )^2",d4=148,kK="<>",gO="real_nonlinear_prover",lk="(%a)-(%a)",kj=" [*] ",k0="__wit",k1=" ,",e9=115,ki="Zero",e4=120,gN="Lia",lj="scale_term : not implemented",kJ="]",gJ=117,bP=166,kI=" \n",kZ='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',e8=128,li="parse_zop",lh="Zmicromega",kH="0",lg=")-(",aW=248,gH="%a * %a",bw=164,bi=167,lf=">",d3=" + ",A="Coq",e3=112,kY=")+(",kG="AProof : ",kh="<linear_prover",gM="$i",ag="$t",e2=182,kW="[",kX="CProof : %a",O="Tauto",kg=132,kF="%a + %a",kV=" Cannot find witness",kE="the use of a specialized external tool called csdp. \n\n",kU=157,kf="PsatzZ",le="Rdefinitions",kD="Timeout",kT="D",kS=" Skipping what remains of this tactic: the complexity of the goal requires ",ld="A(",kB="psatz_Q",kC='" exited ',ke=216,kz="psatz_Z",kA="Rpow_def",ky="nat",kR=154,lc=205,gF="pure_sos",e7=195,af="VarMap",kx=142,kd="%i ",kw="(%a)+(%a)",la="}",lb="monoid",j=250,kv="buggy certificate",W="ZMicromega",kc="C0",bQ="plugins/micromega/certificate.ml",kb="compare_num",d=246,ku=102,d1=151,kQ=204,e6=113,ka="ZArithRing",eZ=208,kt="{",j$="AProof : %a\n",ks="",aX=149,bv="RingMicromega",j_=134,j9="C1",gI="real nonlinear prover",kr=100,k$="%a %s %s",k_="=<",kP="positive",j8="__arith",d2="Reals",j6=", ",gE="<=",j7="QArith",eY=438,kq=" -> ",k9="psatz_R",gL="[%a]",kp="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",k8="Bad logical fragment",j5=215,gD="plugins/micromega/mfourier.ml",ko=206,k7=171,kn=127,kO="CProof : ",eX=196,k6="EnvRing",kN="Refl",j4="t",e1=209,k4="linear prover",k5="Depth",e5=114,gC="%i",km=")^(",k3="}\n",kl=146,G=A9.jsoo_runtime,y=G.caml_check_bound,V=G.caml_equal,j1=G.caml_float_compare,aV=G.caml_fresh_oo_id,Z=G.caml_int_compare,j3=G.caml_int_of_string,gB=G.caml_lessthan,A6=G.caml_list_of_js_array,j2=G.caml_ml_string_length,eW=G.caml_mul,b=G.caml_new_string,i=G.caml_obj_tag,aU=G.caml_register_global,v=G.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):G.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):G.caml_call_gen(a,[b,c])}function
e(a,b,c,d){return a.length==3?a(b,c,d):G.caml_call_gen(a,[b,c,d])}function
x(a,b,c,d,e){return a.length==4?a(b,c,d,e):G.caml_call_gen(a,[b,c,d,e])}function
aT(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):G.caml_call_gen(a,[b,c,d,e,f])}function
C(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):G.caml_call_gen(a,[b,c,d,e,f,g])}function
d0(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):G.caml_call_gen(a,[b,c,d,e,f,g,h])}function
bO(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):G.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
A8(a,b,c,d,e,f,g,h,i,j,k){return a.length==10?a(b,c,d,e,f,g,h,i,j,k):G.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k])}function
A7(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):G.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
u=G.caml_get_global_data(),ee=[0,0,0],A5=[12,44,[2,0,[11,b(")\n"),0]]],em=[0,0],cS=[0,0,0],iU=[0,b(A),[0,b("Logic"),[0,b("Decidable"),0]]],f_=A6([[0,b(A),[0,b("Lists"),[0,b("List"),0]]],[0,b(W),0],[0,b(O),0],[0,b(bv),0],[0,b(k6),0],[0,b(A),[0,b(J),[0,b(W),0]]],[0,b(A),[0,b(J),[0,b("RMicromega"),0]]],[0,b(A),[0,b(J),[0,b(O),0]]],[0,b(A),[0,b(J),[0,b(bv),0]]],[0,b(A),[0,b(J),[0,b(k6),0]]],[0,b(A),[0,b(j7),[0,b("QArith_base"),0]]],[0,b(A),[0,b(d2),[0,b(le),0]]],[0,b(A),[0,b(d2),[0,b(kA),0]]],[0,b("LRing_normalise"),0]]),iW=[0,[0,b(A),[0,b("Numbers"),[0,b("BinNums"),0]]],0],iX=[0,[0,b(A),[0,b(d2),[0,b(le),0]]],[0,[0,b(A),[0,b(d2),[0,b(kA),0]]],[0,[0,b(A),[0,b(d2),[0,b("Raxioms"),0]]],[0,[0,b(A),[0,b(j7),[0,b("Qreals"),0]]],0]]]],iY=[0,[0,b(A),[0,b("ZArith"),[0,b("BinInt"),0]]],0],jP=b(".csdp.cache"),ae=b("micromega_plugin"),l=u.Pervasives,f=u.Num,n=u.Printf,q=u.Big_int,D=u.Unix,d$=u.Marshal,d_=u.Printexc,g=u.List,bj=u.Hashtbl,am=u.Assert_failure,gT=u.Ratio,e_=u.Failure,d9=u.Set,M=u.Not_found,bF=u.Map,bn=u.CErrors,iC=u.String,h=u.CamlinternalLazy,F=u.Names,jB=u.Constr,m=u.EConstr,jL=u.CAst,al=u.Tactics,aR=u.Tacmach,I=u.Tacticals,bs=u.Pp,dX=u.Proofview,jk=u.Invalid_argument,cT=u.Coqlib,f8=u.Goptions,w=u.Ltac_plugin,T=u.Genarg,U=u.Loc,gA=u.Stdarg,lo=b(kH),lp=[0,[12,118,[2,0,0]],b("v%s")],lq=[0,[11,b("1/("),[15,[12,41,0]]],b("1/(%a)")],lr=[0,[11,b("- ("),[15,[12,41,0]]],b("- (%a)")],ls=[0,[12,40,[15,[11,b(kY),[15,[12,41,0]]]]],b(kw)],lt=[0,[12,40,[15,[11,b(lg),[15,[12,41,0]]]]],b(lk)],lu=[0,[12,40,[15,[11,b(")*("),[15,[12,41,0]]]]],b("(%a)*(%a)")],lv=[0,[12,40,[15,[11,b(")/("),[15,[12,41,0]]]]],b("(%a)/(%a)")],lw=[0,[12,40,[15,[11,b(km),[4,3,0,0,[12,41,0]]]]],b("(%a)^(%i)")],lx=[0,[11,b("Aeq("),[4,3,0,0,[12,41,0]]],b("Aeq(%i)")],ly=[0,[11,b("Ale("),[4,3,0,0,[12,41,0]]],b("Ale(%i)")],lz=[0,[11,b("Alt("),[4,3,0,0,[12,41,0]]],b("Alt(%i)")],lA=[0,[11,b("eq("),[2,0,[12,41,0]]],b("eq(%s)")],lB=[0,[11,b("le("),[2,0,[12,41,0]]],b("le(%s)")],lC=[0,[11,b("lt("),[2,0,[12,41,0]]],b("lt(%s)")],lD=[0,[12,40,[15,[11,b(")^2"),0]]],b("(%a)^2")],lE=[0,[11,b(lb),0],b(lb)],lF=[0,[15,[11,b(ch),[15,0]]],b(gH)],lG=[0,[15,[11,b(d3),[15,0]]],b(kF)],lH=[0,[15,[11,b(ch),[15,0]]],b(gH)],lK=b(";"),lS=b("map3"),ml=[0,[11,b(e0),[2,0,[11,b(kC),[4,3,0,0,0]]]],b('command "%s" exited %i')],mk=[0,[11,b(e0),[2,0,[11,b(kC),[2,0,0]]]],b('command "%s" exited %s')],mm=[0,[11,b(e0),[2,0,[11,b('" killed '),[4,3,0,0,0]]]],b('command "%s" killed %i')],mn=[0,[11,b(e0),[2,0,[11,b('" stopped '),[4,3,0,0,0]]]],b('command "%s" stopped %i')],mb=[0,b("plugins/micromega/mutils.ml"),289,7],l7=b("select_pos"),l0=[0,0,0],lX=b("list_fold_right_elements"),lV=b("try_find"),lN=b("from_option"),nM=[0,0],nX=[0,0,0],nY=[0,[0,0],0],nG=[0,0],nF=[0,0],nE=[0,0],nw=[0,[0,0]],nx=[0,[0,0]],ny=[0,[0,0]],nz=[0,[0,0]],ns=[0,[0,0]],nt=[0,[0,0]],nu=[0,[0,0]],nv=[0,[0,0]],m_=[0,[0,0],0],m8=[0,0,0],m1=[0,1],m2=[0,2],m3=[0,3],mW=[0,0],mY=[0,0],mX=[0,1],m0=[0,3],mZ=[0,0],mC=[0,[1,0]],mD=[0,0,[0,0]],mE=[0,[0,0],0],mF=[0,0],mG=[0,[1,0]],mH=[0,[1,0]],mI=[0,0],mJ=[0,[1,0]],mK=[0,[1,0]],mL=[0,[1,0]],mM=[0,0],mN=[0,[1,0]],mO=[0,0,0],mP=[0,0,0],mQ=[0,0],mR=[0,0,0],mS=[0,0],my=[1,0],mx=[0,0],mp=[1,0],mq=[1,0],mr=[0,0],mt=[0,0],ms=[0,0],nd=[0,0],nq=[0,0],nK=[0,0],nO=[0,[0,0],0],nP=[0,0,0],nQ=[0,[0,0],0],nR=[0,0,0],nS=[0,[0,0],0],nT=[0,0,0],nU=[0,0,0],nV=[0,0,0],nZ=[0,[0,0],0],n0=[0,0,0],n1=[0,[0,0],0],n2=[0,0,0],n3=[0,[0,0],0],n4=[0,0,0],n5=[0,0,0],n6=[0,0,0],oQ=[0,[11,b(ki),0],b(ki)],oR=[0,[11,b("Hyp "),[4,3,0,0,0]],b("Hyp %i")],oS=[0,[11,b("Def "),[4,3,0,0,0]],b("Def %i")],oT=[0,[11,b("Cst "),[2,0,0]],b("Cst %s")],oU=[0,[11,b(kL),0],b(kL)],oV=[0,[11,b("P * "),[15,0]],b("P * %a")],oW=[0,[12,40,[15,[11,b(")/"),[2,0,0]]]],b("(%a)/%s")],oX=[0,[15,[11,b(ch),[15,0]]],b(gH)],oY=[0,[15,[11,b(d3),[15,0]]],b(kF)],oZ=[0,[12,91,[15,[12,93,0]]],b(gL)],o0=[0,[12,46,0],b(".")],o1=[0,[4,3,0,0,[11,b(":= "),[15,[11,b(" ; "),[15,0]]]]],b("%i:= %a ; %a")],o2=[0,[4,3,0,0,[12,123,[15,[11,b(gE),[15,[11,b(gE),[15,[12,125,[15,0]]]]]]]]],b("%i{%a<=%a<=%a}%a")],pg=[0,0],pf=[0,[11,b("apply_pivot -> {"),[15,[11,b(k3),0]]],b("apply_pivot -> {%a}\n")],pe=[0,[11,b("xpivot_eq {"),[15,[11,b("} "),[15,[12,32,[2,0,[11,b(" {"),[15,[11,b(k3),0]]]]]]]]],b("xpivot_eq {%a} %a %s {%a}\n")],pd=[0,0],pc=[0,[11,b("mult "),[2,0,[12,32,[15,[11,b(" ("),[15,[12,44,[2,0,[11,b(") -> ("),[15,A5]]]]]]]]]],b("mult %s %a (%a,%s) -> (%a,%s)\n")],pb=[0,0],pa=[0,0,[0,0]],o$=[0,0,[0,0]],o9=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],b(k$)],o8=[0,[2,0,[12,46,[15,[11,b(" +"),0]]]],b("%s.%a +")],o5=[0,b("plugins/micromega/polynomial.ml"),544,10],o4=[0,[11,b("normalise_proof "),[15,[11,b(kq),[15,0]]]],b("normalise_proof %a -> %a")],oN=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],b(k$)],oL=b(gK),oK=b(kk),oC=b(kb),oD=b(kb),oG=[0,0],oB=[0,0],oy=[0,0],ow=[0,[2,0,[12,e4,[4,3,0,0,[11,b(d3),0]]]],b("%sx%i + ")],on=[0,0],om=[0,1],ok=[0,0],oi=[0,[2,0,[12,32,0]],b("%s ")],oj=[0,[2,0,[12,42,[15,[12,32,0]]]],b("%s*%a ")],n_=[0,[12,e4,[4,3,0,0,[12,46,0]]],b("x%i.")],n$=[0,[12,e4,[4,3,0,0,[12,94,[4,3,0,0,[12,46,0]]]]],b("x%i^%i.")],oz=[0,1],pr=[0,[12,72,[4,3,0,0,0]],b("H%i")],ps=[0,[11,b("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],b("E(%i,%a,%a)")],pt=[0,[11,b(ld),[15,[12,44,[15,[12,41,0]]]]],b("A(%a,%a)")],qu=[0,1],qv=[0,[0,0,0]],qr=b("merge_proof : pivot is not possible"),qs=[0,1],qt=[0,1],qq=[0,0],qk=[0,1],ql=[0,1],qm=[0,1],qn=[0,1],qo=[0,1],qp=[0,1],qg=[0,0],qh=[0,-1],qi=[0,[11,b("optimise Exception : "),[2,0,0]],b("optimise Exception : %s")],qe=[0,0,0],p7=[0,0],p8=[0,0],p9=[0,0],p_=[0,0],p$=[0,0],qa=[0,0],qb=[0,0],qc=[0,0],p6=[0,0],p5=b("bound_of_variable: impossible"),p4=[0,0,0],p2=[0,0,0],p3=b("SystemContradiction"),p1=[0,0],p0=[0,[4,3,0,0,[11,b(kq),[2,0,[12,10,0]]]],b("%i -> %s\n")],pX=[0,0],pW=[0,0,0,0],pU=[0,0],pS=[0,0],pT=[0,0],pV=[0,b(gD),309,4],pR=[0,b(gD),261,9],pO=[0,1],pP=[0,0],pL=[0,b(gD),199,4],pK=[0,[11,b("(val x = "),[2,0,[11,b(k1),[15,[12,44,[2,0,[12,41,0]]]]]]],b("(val x = %s ,%a,%s)")],pF=[0,[2,0,[11,b(" <= "),0]],b("%s <= ")],pG=[0,[11,b(gE),[2,0,[12,10,0]]],b("<=%s\n")],pH=b("\n"),pC=[0,[4,3,0,0,[12,32,0]],b(kd)],pB=b(kt),pD=b(la),py=[0,[4,3,0,0,[12,32,0]],b(kd)],px=b(kt),pz=b(la),pv=[0,[12,40,[15,[12,44,[15,[12,41,0]]]]],b("(%a,%a)")],pu=b("oo"),pm=[0,1],pp=b("Mfourier.SystemContradiction"),pN=b("Mfourier.TimeOut"),rl=b(lj),rm=b("scale term: not implemented"),rn=b(lj),rq=[0,0],rr=b("term_to_q_expr: not implemented"),rw=[0,0],rx=b("term_to_z_expr: not implemented"),rD=b("Cuts should already be compiled"),sd=[0,0],se=[0,1],sf=[0,0],sg=[0,1],sh=[0,b(bQ),1278,1],r6=[0,[11,b(ll),0],b(ll)],r8=[0,b(bQ),1177,2],r7=[0,[11,b("Found interval: "),[15,[11,b(" in ["),[2,0,[12,59,[2,0,[11,b("] -> "),0]]]]]]],b("Found interval: %a in [%s;%s] -> ")],r9=[0,0],r_=[0,1],sb=[0,b(bQ),1204,2],r$=[0,[11,b("xlia:  "),[15,[11,b(kI),0]]],b("xlia:  %a \n")],sa=[0,[11,b("after reduction:  "),[15,[11,b(kI),0]]],b("after reduction:  %a \n")],r4=[0,[11,b("Found a new bound "),[15,0]],b("Found a new bound %a")],r2=[0,1],r3=[0,0,0],r5=b("Interval without proof"),r1=[0,[11,b("Bad news : loss of completeness "),[15,[12,61,[2,0,0]]]],b("Bad news : loss of completeness %a=%s")],rZ=[0,0],rX=[0,1],rY=[0,-1],rV=[0,1],rW=[0,-1],rR=[0,[2,0,[11,b(ch),[2,0,[11,b(d3),[2,0,[11,b(ch),[2,0,[11,b(" = "),[2,0,[12,10,0]]]]]]]]]],b("%s * %s + %s * %s = %s\n")],rM=[0,1],rN=[0,b(bQ),819,5],rO=[0,0],rK=[0,[11,b(kh),0],b(kh)],rL=[0,[12,62,0],b(lf)],rJ=b("proof_of_farkas : not enough hyps"),rF=[0,[11,b(kG),[15,[12,10,0]]],b(j$)],rG=[0,[11,b(kO),[15,0]],b(kX)],rE=[0,[11,b("compiled proof "),[15,[12,10,0]]],b("compiled proof %a\n")],rC=[0,[0,0]],rB=b("id_of_hyp"),rz=[0,0],rA=[0,1],rv=[0,0],rs=[0,1],rt=[0,0],rp=b("bad index"),rk=b("pexpr_of_cstr_compat"),ri=[0,b(bQ),514,9],rg=[0,b(bQ),495,12],q$=[0,0],q7=b("cannot happen"),q4=[0,[11,b(kG),[15,[12,10,0]]],b(j$)],q5=[0,[11,b(kO),[15,0]],b(kX)],q3=[0,[11,b("raw certificate "),[2,0,0]],b("raw certificate %s")],qY=b("make_certificate(1)"),qZ=b("empty_certificate"),qT=b("= 0"),qU=b("<> 0"),qV=b("> 0"),qW=b(">= 0"),qR=[0,1],qQ=[0,0],qL=[0,0],qM=[0,[0,0]],qN=[0,0],qP=[0,b(bQ),ku,1],qO=[0,0],qK=[0,[0,0]],qJ=[0,[0,0]],qB=[0,0],qG=[0,[0,0],0],qH=[0,0,0],q0=b("Certificate.Found"),q2=b("Certificate.Strict"),rP=b("Certificate.FoundProof"),rS=b("Certificate.Result"),sq=[0,0,0],so=[0,0,0],sm=[0,0,[0,5,0]],sp=[0,1,[0,4,[0,5,0]]],sn=[0,1,[0,6,[0,5,0]]],sk=[0,1,[0,6,[0,5,0]]],si=b("Persistent_cache.PHashtable(Key).InvalidTableFormat"),sj=b("Persistent_cache.PHashtable(Key).UnboundTable"),sD=b("tt"),sE=b("ff"),sF=b("X "),sG=[0,[11,b(ld),[15,[12,41,0]]],b("A(%a)")],sH=[0,[11,b("C("),[15,[12,44,[15,[12,41,0]]]]],b("C(%a,%a)")],sI=[0,[11,b("D("),[15,[12,44,[15,[12,41,0]]]]],b("D(%a,%a)")],sJ=[0,[11,b("N("),[15,[12,41,0]]],b("N(%a)")],sL=b(ks),sK=[0,[11,b("I("),[15,[2,0,[12,44,[15,[12,41,0]]]]]],b("I(%a%s,%a)")],xe=[0,0],xs=b("[]"),xt=[0,[12,91,[15,[12,93,0]]],b(gL)],xu=[0,[12,91,[15,[11,b(j6),[15,[11,b(j6),[15,[12,93,0]]]]]]],b("[%a, %a, %a]")],xw=[0,[12,68,0],b(kT)],xx=[0,[11,b("R["),[15,[12,44,[15,[12,93,0]]]]],b("R[%a,%a]")],xy=[0,[11,b("C["),[15,[12,44,[15,[12,93,0]]]]],b("C[%a,%a]")],xz=b(kJ),xA=b(kW),xB=[0,[11,b("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],b("EP[%a,%a,%a]")],xQ=b("abstract_wrt_formula"),yQ=b(gO),yO=b(gO),yL=b(gO),yF=b(gI),yE=b(gI),yD=b(gI),yo=b(kv),yn=b(kv),yh=b("csdpcert"),yi=b(J),yj=b("plugins"),yb=b(j8),yc=[0,0],yd=b(kV),x7=b(k8),x8=b(kD),x9=b(kp),x_=b(kZ),x$=b(kE),ya=b(kS),x1=b(k0),x2=b(j4),x3=[0,[0,b(A),[0,b(J),[0,b(af),0]]],[0,[0,b(af),0],0]],x4=b(af),x5=b(kM),x6=b(lm),xY=b(j8),xZ=[0,0],x0=b(kV),xS=b(k8),xT=b(kD),xU=b(kp),xV=b(kZ),xW=b(kE),xX=b(kS),xO=b("bad old index"),xP=b("proof compaction error"),xM=[0,[15,[11,b(" ;"),0]],b("%a ;")],xL=b(kW),xN=b(kJ),xJ=[0,0],xG=b(k0),xH=b(kM),xI=b(lm),xv=[0,[15,[12,47,[15,0]]],b("%a/%a")],xp=b(j4),xq=[0,[0,b(A),[0,b(J),[0,b(af),0]]],[0,[0,b(af),0],0]],xr=b(af),xm=b("Empty"),xn=[0,[0,b(A),[0,b(J),[0,b(af),0]]],[0,[0,b(af),0],0]],xo=b(af),xj=b("Leaf"),xk=[0,[0,b(A),[0,b(J),[0,b(af),0]]],[0,[0,b(af),0],0]],xl=b(af),xg=b("Node"),xh=[0,[0,b(A),[0,b(J),[0,b(af),0]]],[0,[0,b(af),0],0]],xi=b(af),vU=b(kc),vV=b(j9),vW=b("CQ _"),vX=[0,[12,40,[15,[11,b(d3),[15,[12,41,0]]]]],b("(%a + %a)")],vY=[0,[12,40,[15,[11,b(" - "),[15,[12,41,0]]]]],b("(%a - %a)")],vZ=[0,[12,40,[15,[11,b(ch),[15,[12,41,0]]]]],b("(%a * %a)")],v0=[0,[11,b("(/ "),[15,[12,41,0]]],b("(/ %a)")],v1=[0,[11,b("(- "),[15,[12,41,0]]],b("(- %a)")],wU=[0,0,0],xb=[0,[11,b(ln),[4,3,0,0,0]],b(k2)],xa=[0,[11,b(ln),[4,3,0,0,0]],b(k2)],w$=[0,[11,b("__x"),[4,3,0,0,0]],b("__x%i")],w9=[0,b("plugins/micromega/coq_micromega.ml"),1391,11],wX=b("error : parse_arith(2)"),wW=b("parse_qexpr parse error"),wV=[0,0],wF=b("get_rank"),wC=[1,b("Oups")],wA=b(li),wz=b(li),ww=[0,[12,40,[15,[12,32,[15,[12,32,[15,[12,41,0]]]]]]],b("(%a %a %a)")],wp=[0,[12,61,0],b(kk)],wq=[0,[11,b(kK),0],b(kK)],wr=[0,[11,b(k_),0],b(k_)],ws=[0,[11,b(gK),0],b(gK)],wt=[0,[12,60,0],b("<")],wu=[0,[12,62,0],b(lf)],wi=[0,[12,48,0],b(kH)],wj=[0,[11,b("(In "),[15,[12,41,[12,37,[11,b(ky),0]]]]],b("(In %a)%%nat")],wk=[0,[12,40,[15,[11,b("^2)"),0]]],b("(%a^2)")],wl=[0,[11,b("( "),[15,[11,b(kj),[15,[12,41,0]]]]],b("( %a [*] %a)")],wm=[0,[12,40,[15,[11,b(kj),[15,[12,41,0]]]]],b("(%a [*] %a)")],wn=[0,[12,40,[15,[11,b(" [+] "),[15,[12,41,0]]]]],b("(%a [+] %a)")],wo=[0,[12,40,[15,[12,41,[12,37,[11,b(kP),0]]]]],b("(%a)%%positive")],wf=[0,[12,91,[15,[12,93,0]]],b(gL)],we=[0,[12,40,[15,[12,32,[12,64,[15,[12,41,0]]]]]],b("(%a @%a)")],wa=[0,[11,b("Pc "),[15,0]],b("Pc %a")],wb=[0,[11,b("Pinj("),[15,[12,44,[15,[12,41,0]]]]],b("Pinj(%a,%a)")],wc=[0,[11,b("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],b("PX(%a,%a,%a)")],v6=[0,[11,b("V "),[15,0]],b("V %a")],v7=[0,[12,40,[15,[11,b(kY),[15,[12,41,0]]]]],b(kw)],v8=[0,[12,40,[15,[11,b(lg),[15,[12,41,0]]]]],b(lk)],v9=[0,[15,[11,b("*("),[15,[12,41,0]]]],b("%a*(%a)")],v_=[0,[11,b("-("),[15,[12,41,0]]],b("-(%a)")],v$=[0,[12,40,[15,[11,b(km),[15,[12,41,0]]]]],b("(%a)^(%a)")],v3=[0,[15,[11,b(k1),[15,0]]],b("%a ,%a")],v4=[0,[15,0],b("%a")],v5=[0,[2,0,[15,[2,0,0]]],b("%s%a%s")],vS=[0,[2,0,0],b("%s")],vQ=[0,[4,3,0,0,0],b(gC)],vO=[0,[4,3,0,0,0],b(gC)],vN=[0,[4,3,0,0,0],b(gC)],vJ=b("ukn"),vK=b("BadTerm"),vL=b("Goal"),vE=b("Formula"),vF=[0,[0,b(A),[0,b(J),[0,b(bv),0]]],[0,[0,b(bv),0],0]],vG=b(bv),vB=b("Build_Formula"),vC=[0,[0,b(A),[0,b(J),[0,b(bv),0]]],[0,[0,b(bv),0],0]],vD=b(bv),vx=b("N_of_Z"),vy=[0,[0,b(A),[0,b("setoid_ring"),[0,b(ka),0]]],0],vz=b(ka),vt=b("ZWitness"),vu=[0,[0,b(A),[0,b(J),[0,b(W),0]]],0],vv=b(gG),vp=b("QWitness"),vq=[0,[0,b(A),[0,b(J),[0,b(gG),0]]],0],vr=b(gG),vl=b("BFormula"),vm=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],vn=b(W),vi=b("I"),vj=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],vk=b(W),vf=b("X"),vg=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],vh=b(W),vc=b("A"),vd=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],ve=b(W),u$=b("N"),va=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],vb=b(W),u8=b(kT),u9=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],u_=b(W),u5=b("Cj"),u6=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],u7=b(W),u2=b("FF"),u3=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],u4=b(W),uZ=b("TT"),u0=[0,[0,b(A),[0,b(J),[0,b(O),0]]],[0,[0,b(O),0],0]],u1=b(W),uV=b("make_conj"),uW=[0,[0,b(kN),0],0],uX=b(lh),uR=b("make_impl"),uS=[0,[0,b(kN),0],0],uT=b(lh),uP=b("coneMember"),uO=b(kf),uN=b("PsatzC"),uM=b("PsatzAdd"),uL=b("PsatzMulC"),uK=b("PsatzMulE"),uJ=b("PsatzSquare"),uI=b("PsatzIn"),uH=b("OpGt"),uG=b("OpGe"),uF=b("OpLt"),uE=b("OpLe"),uD=b("OpNEq"),uC=b("OpEq"),uB=b("Pinj"),uA=b("Pc"),uz=b("PX"),uy=b("PEpow"),ux=b("PEsub"),uw=b("PEmul"),uv=b("PEopp"),uu=b("PEadd"),ut=b("PEc"),us=b("PEX"),ur=b("Q2R"),uq=b("IZR"),up=b("pow"),uo=b("Rinv"),um=b("Rdiv"),ul=b("Rmult"),uk=b("Ropp"),uj=b("Rminus"),ui=b("Rplus"),uh=b("Rlt"),ug=b("Rle"),uf=b("Rge"),ue=b("Rgt"),ud=b("Qpower"),uc=b("Qmult"),ub=b("Qopp"),ua=b("Qminus"),t$=b("Qplus"),t_=b("Qeq"),t9=b("Qlt"),t8=b("Qle"),t6=b("Qge"),t4=b("Qgt"),t3=b("Z.pow"),t2=b("Z.mul"),t1=b("Z.opp"),t0=b("Z.sub"),tZ=b("Z.add"),tY=b("eq"),tX=b("Z.lt"),tW=b("Z.le"),tV=b("Z.ge"),tU=b("Z.gt"),tS=b("EnumProof"),tQ=b("CutProof"),tO=b("RatProof"),tM=b("DoneProof"),tK=b("ZArithProof"),tJ=b("R1"),tI=b("R0"),tH=b("COpp"),tG=b("CInv"),tF=b("CMult"),tE=b("CMinus"),tD=b("CPlus"),tC=b("CZ"),tB=b("CQ"),tA=b(j9),tz=b(kc),tx=b("Rcst"),tw=b("Qmake"),tu=b("Build_Witness"),tt=b("R"),ts=b("Q"),tr=b("Zneg"),tq=b("Zpos"),tp=b("Z0"),to=b("Z"),tn=b("xI"),tm=b("xO"),tl=b("xH"),tj=b(kP),th=b("option"),tf=b("None"),te=b("pair"),td=b("Npos"),tc=b("N0"),ta=b(ky),s$=b("S"),s_=b("O"),s8=b("list"),s7=b("nil"),s6=b("cons"),s5=b("False"),s4=b("True"),s3=b("iff"),s2=b("not"),s1=b("or"),s0=b("and"),sN=[0,0,0],su=b(ks),st=[0,[11,b("time "),[2,0,[12,32,[8,0,0,0,[12,10,0]]]]],b("time %s %f\n")],sw=[0,b(gN),[0,b("Enum"),0]],sx=b("Lia Enum"),sz=[0,b("Lra"),[0,b(k5),0]],sB=[0,b(gN),[0,b(k5),0]],sU=b(W),sV=b(W),sW=b(W),sX=b(W),sY=b(W),sZ=b(W),vM=b("Coq_micromega.M.ParseError"),xR=b("Coq_micromega.CsdpNotFound"),ye=b("csdp"),yq=b(".lia.cache"),ys=b(".nia.cache"),yu=b(".nra.cache"),yx=b(k4),yA=b(k4),yC=b("nra"),yG=b("lia"),yH=b("nlia"),yR=b(gF),yU=b(gF),yX=b(gF),y4=[0,b("myred"),0],y6=b("RED"),y9=b(ag),za=b(kz),zd=b(ag),zh=b(gM),zk=b(kz),zm=b(kf),zp=b(ag),zs=b("xlia"),zu=b(gN),zx=b(ag),zA=b("xnlia"),zC=b("Nia"),zF=b(ag),zI=b("xnra"),zK=b("NRA"),zN=b(ag),zQ=b("xnqa"),zS=b("NQA"),zV=b(ag),zY=b("sos_Z"),z0=b("Sos_Z"),z3=b(ag),z6=b("sos_Q"),z8=b("Sos_Q"),z$=b(ag),Ac=b("sos_R"),Ae=b("Sos_R"),Ah=b(ag),Ak=b("lra_Q"),Am=b("LRA_Q"),Ap=b(ag),As=b("lra_R"),Au=b("LRA_R"),Ax=b(ag),AA=b(k9),AD=b(ag),AH=b(gM),AK=b(k9),AM=b("PsatzR"),AP=b(ag),AS=b(kB),AV=b(ag),AZ=b(gM),A2=b(kB),A4=b("PsatzQ"),sl=u.End_of_file,yg=u.Coq_config,yk=u.Envars,yl=u.Filename,xE=u.Evd,xF=u.Termops,w2=u.Retyping,w3=u.Sorts,wH=u.Pfedit,wy=u.Reductionops,sS=u.Universes,yf=u.System,y2=u.Mltop;function
ah(d,b){if(typeof
b==="number")return c(l[54],d,lo);else
switch(b[0]){case
0:var
m=a(f[40],b[1]);return c(l[54],d,m);case
1:return e(n[1],d,lp,b[1]);case
2:return x(n[1],d,lq,ah,b[1]);case
3:return x(n[1],d,lr,ah,b[1]);case
4:var
g=b[1];return C(n[1],d,ls,ah,g[1],ah,g[2]);case
5:var
h=b[1];return C(n[1],d,lt,ah,h[1],ah,h[2]);case
6:var
i=b[1];return C(n[1],d,lu,ah,i[1],ah,i[2]);case
7:var
j=b[1];return C(n[1],d,lv,ah,j[1],ah,j[2]);default:var
k=b[1];return aT(n[1],d,lw,ah,k[1],k[2])}}function
bR(d,b){switch(b[0]){case
0:return e(n[1],d,lx,b[1]);case
1:return e(n[1],d,ly,b[1]);case
2:return e(n[1],d,lz,b[1]);case
3:var
g=a(f[40],b[1]);return e(n[1],d,lA,g);case
4:var
h=a(f[40],b[1]);return e(n[1],d,lB,h);case
5:var
i=a(f[40],b[1]);return e(n[1],d,lC,i);case
6:return x(n[1],d,lD,ah,b[1]);case
7:return c(n[1],d,lE);case
8:return C(n[1],d,lF,ah,b[1],bR,b[2]);case
9:return C(n[1],d,lG,bR,b[1],bR,b[2]);default:return C(n[1],d,lH,bR,b[1],bR,b[2])}}var
gP=[0,ah,bR];aU(680,gP,"Micromega_plugin.Sos_types");var
lI=0;function
lJ(e,b,d){var
a=d;for(;;){if(a){var
f=a[2];c(e,b,a[1]);c(l[54],b,lK);var
a=f;continue}return 0}}function
gQ(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=v(b);try{a(c,0)}catch(a){throw b}throw b}}function
lL(c,b){return b?[0,a(c,b[1])]:0}function
lM(b){return b?b[1]:a(l[2],lN)}function
lO(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1][1],d);if(c)return c;var
b=f;continue}return 0}}function
lP(e,d){var
b=0,a=d;for(;;){if(a){var
f=a[2];c(e,b,a[1]);var
b=b+1|0,a=f;continue}return 0}}function
lQ(h,f){var
b=0,a=f;for(;;){if(a){var
d=a[2],j=a[1],i=function(d){return function(b,a){return[0,c(h,d,a),b]}}(j),b=e(g[20],i,b,d),a=d;continue}return b}}function
lR(f,d){var
b=0,a=d;for(;;){if(a){var
i=a[2],j=a[1],h=function(d){return function(b,a){return[0,c(f,d,a),b]}}(j),b=e(g[20],h,b,a),a=i;continue}return b}}function
gR(f,d,c,b){if(d){if(c)if(b){var
g=b[1],h=c[1],i=d[1],j=gR(f,d[2],c[2],b[2]);return[0,e(f,i,h,g),j]}}else
if(!c)if(!b)return 0;return a(l[1],lS)}function
lT(g,f,e){var
b=f,a=e;for(;;){if(b){var
h=b[2],i=b[1];if(a){var
d=a[2];if(c(g,i,a[1])){var
b=h,a=d;continue}var
a=d;continue}return 0}return 1}}function
lU(c){return function(d){var
b=d;for(;;){if(b){var
e=b[2],f=b[1];try{var
g=a(c,f);return g}catch(a){a=v(a);if(a[1]===e_){var
b=e;continue}throw a}}return a(l[2],lV)}}}function
lW(g,b){function
d(b){if(b){var
e=b[2],f=b[1];return e?c(g,f,d(e)):f}return a(l[1],lX)}return d(b)}function
lY(e,d){var
a=[0,0,d];for(;;){var
b=a[2],c=a[1];if(b<e)return c;var
a=[0,[0,b,c],b-1|0];continue}}function
lZ(h,b){function
c(e,b){var
c=e[2],d=e[1];if(d)return[0,d,[0,b,c]];var
f=a(h,b);return f?[0,[0,[0,f[1],b]],c]:[0,d,[0,b,c]]}return e(g[20],c,l0,b)}function
gS(d,b){var
a=c(q[17],d,b),e=c(q[15],d,a),f=c(q[15],b,a),g=c(q[10],e,f);return c(q[10],a,g)}function
d5(b){return 2===b[0]?a(gT[3],b[1]):q[2]}function
d6(b){switch(b[0]){case
0:return a(q[36],b[1]);case
1:return b[1];default:return a(gT[2],b[1])}}function
gU(d,c){var
b=d,a=c;for(;;){if(a){var
e=a[2],b=gS(b,d5(a[1])),a=e;continue}return b}}function
gV(e,d){var
b=e,a=d;for(;;){if(a){var
f=a[2],g=d6(a[1]),b=c(q[17],b,g),a=f;continue}return b}}function
l1(b){var
a=gV(q[1],b);return 0===c(q[23],a,q[1])?q[2]:a}function
l2(a){var
b=gU(q[2],a);function
d(a){var
d=d5(a),e=d6(a),f=c(q[10],e,b);return c(q[15],f,d)}return c(g[17],d,a)}function
e$(e,a){function
b(d,a){if(a){var
f=a[1],g=b(d+1|0,a[2]);return[0,c(e,f,d),g]}return 0}return b(0,a)}function
l3(c,b){var
d=e$(function(d,b){return[0,b,a(c,d)]},b);return a(g[9],d)}function
gW(c,b){var
d=c+a(g[1],b)|0;return[0,e$(function(b,a){return[0,b,a+c|0]},b),d]}function
l4(a){function
b(e,a){if(a){var
c=a[1],f=a[2],g=c[1],d=gW(e,c[2]),h=d[1];return[0,[0,g,h],b(d[2],f)]}return 0}return b(0,a)}function
l5(h,a){function
b(i){var
a=i;for(;;){if(a){var
d=a[2],e=a[1],f=e[2],j=e[1],k=function(a){return a[2]},l=c(g[17],k,f),m=function(a){return c(g[31],a,h)};if(c(g[28],m,l))return[0,[0,j,f],b(d)];var
a=d;continue}return 0}}return b(a)}function
l6(c,b){function
e(h,d,g){var
c=h,b=g;for(;;){if(d){var
i=d[2],j=d[1];if(b){var
f=b[2],k=b[1];if(c===j)return[0,k,e(c+1|0,i,f)];var
c=c+1|0,b=f;continue}return a(l[2],l7)}return 0}}return e(0,c,b)}function
gX(a){return a?gX(a[1])+1|0:0}function
bS(a){return typeof
a==="number"?1:0===a[0]?1+(2*bS(a[1])|0)|0:2*bS(a[1])|0}function
l8(a){return a?bS(a[1]):0}function
fa(a){return typeof
a==="number"?1:0===a[0]?1+(2*fa(a[1])|0)|0:2*fa(a[1])|0}function
l9(a){return typeof
a==="number"?0:0===a[0]?bS(a[1]):-bS(a[1])|0}function
ci(a){if(typeof
a==="number")return q[2];else{if(0===a[0]){var
b=ci(a[1]),d=c(q[11],2,b);return c(q[7],1,d)}var
e=ci(a[1]);return c(q[11],2,e)}}function
d7(b){if(typeof
b==="number")return q[1];else{if(0===b[0])return ci(b[1]);var
c=ci(b[1]);return a(q[3],c)}}function
l_(a){return[1,d7(a)]}var
l$=[0,gX,bS,l8,fa,l9,ci,d7,l_,function(a){var
b=a[1],d=[1,d7([0,a[2]])],e=[1,d7(b)];return c(f[9],e,d)}];function
gY(a){return 0===a?0:[0,gY(a-1|0)]}function
bT(a){return 1===a?0:1===(a&1)?[0,bT(a>>>1|0)]:[1,bT(a>>>1|0)]}function
ma(a){if(0<=a)return 0===a?0:[0,bT(a)];throw[0,am,mb]}function
fb(a){return 1===a?0:1===(a&1)?[0,fb(a>>>1|0)]:[1,fb(a>>>1|0)]}function
mc(a){var
b=Z(a,0);return 0===b?0:1===b?[0,bT(a)]:[1,bT(-a|0)]}function
d8(d){var
f=a(q[36],2);function
b(a){if(c(q[24],a,q[2]))return 0;var
d=c(q[14],a,f),e=d[1];return c(q[24],q[2],d[2])?[0,b(e)]:[1,b(e)]}return b(d)}function
gZ(b){var
c=a(q[22],b);return 0===c?0:1===c?[0,d8(b)]:[1,d8(a(q[3],b))]}var
md=[0,gY,bT,ma,fb,mc,d8,gZ,function(a){var
b=d8(d5(a));return[0,gZ(d6(a)),b]}];function
me(d){var
b=d;for(;;){if(b){var
e=b[2],c=a(b[1],0);if(0===c){var
b=e;continue}return c}return 0}}function
mf(g,f,e){var
b=f,a=e;for(;;){if(b){if(a){var
h=a[2],i=b[2],d=c(g,b[1],a[1]);if(0===d){var
b=i,a=h;continue}return d}return 1}return a?-1:0}}var
mg=[0,me,mf,function(e,d){var
b=d,c=0;for(;;){if(b){var
f=b[2],g=a(e,b[1])^c,b=f,c=g;continue}return c^a(bj[21],0)}}];function
mh(a){return a}function
mi(a){return a+1|0}var
g0=[0,mh,mi,function(d,b){var
e=a(l[21],b);return c(l[54],d,e)},Z],mj=a(d9[1],[0,g0[4]]);function
g1(a){for(;;)try{var
d=c(D[15],0,a)[2];return d}catch(a){a=v(a);if(a[1]===D[1]){var
b=a[2];if(typeof
b==="number")if(11===b)continue}throw a}}var
k=[0,lI,lJ,gQ,lL,lM,lO,lP,lQ,lR,gR,lT,lU,lW,lY,lZ,gS,d5,d6,gU,gV,l1,l2,e$,l3,gW,l4,l5,l6,l$,md,mg,g0,mj,g1,function(b,s,r){var
f=c(D[67],0,0),h=f[2],i=f[1],j=c(D[67],0,0),k=j[2],m=j[1],o=c(D[67],0,0),p=o[2],t=o[1],u=aT(D[69],b,s,i,k,p),q=a(D[31],h);c(l[60],q,r);a(l[51],q);var
d=g1(u);function
w(e){var
b=[0,i,[0,h,[0,m,[0,k,[0,t,[0,p,0]]]]]];function
d(b){try{var
c=a(D[24],b);return c}catch(a){return 0}}return c(g[15],d,b)}return gQ(function(p){switch(d[0]){case
0:var
c=d[1];if(0===c){var
f=a(D[30],m);try{var
i=a(d$[3],f);return i}catch(c){c=v(c);var
g=a(d_[1],c),h=e(n[4],mk,b,g);return a(l[2],h)}}var
j=e(n[4],ml,b,c);return a(l[2],j);case
1:var
k=e(n[4],mm,b,d[1]);return a(l[2],k);default:var
o=e(n[4],mn,b,d[1]);return a(l[2],o)}},w)}];aU(691,k,"Micromega_plugin.Mutils");function
bx(a){return 0===a?1:0}function
by(a,b){if(a){var
c=a[1];return[0,c,by(a[2],b)]}return b}function
g2(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
fc(b,a){return b?[0,fc(b[1],a)]:a}var
mo=[0];function
aY(a){return typeof
a==="number"?mp:0===a[0]?[1,aY(a[1])]:[0,a[1]]}function
bU(b,a){if(typeof
b==="number")return typeof
a==="number"?mq:0===a[0]?[1,aY(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,aY(c)]:0===a[0]?[1,cj(c,a[1])]:[0,bU(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,bU(d,a[1])]:[1,bU(d,a[1])]}}function
cj(b,a){if(typeof
b==="number")return typeof
a==="number"?mr:0===a[0]?[0,aY(a[1])]:[1,aY(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,aY(c)]:0===a[0]?[0,cj(c,a[1])]:[1,cj(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,aY(d)]:0===a[0]?[1,cj(d,a[1])]:[0,bU(d,a[1])]}}function
ck(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,ck(a[1])]}function
cl(a){return typeof
a==="number"?0===a?ms:1:[0,[0,a[1]]]}function
cm(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
g3(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,ck(a[1])]]}function
bV(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?cm(bV(c,a[1])):cl(bV(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,ck(d)]:0===a[0]?cl(cn(d,a[1])):cm(bV(d,a[1]))}}function
cn(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,ck(c)]:0===a[0]?cl(cn(c,a[1])):cm(bV(c,a[1]))}var
d=b[1];return typeof
a==="number"?g3(d):0===a[0]?cm(cn(d,a[1])):cl(cn(d,a[1]))}}function
fd(c,b){var
a=bV(c,b);return typeof
a==="number"?0:a[1]}function
fe(b,a){return typeof
b==="number"?a:0===b[0]?bU(a,[1,fe(b[1],a)]):[1,fe(b[1],a)]}function
co(a){return typeof
a==="number"?mt:0===a[0]?[0,co(a[1])]:[0,co(a[1])]}function
g4(h,g,f){var
c=h,b=g,a=f;for(;;)if(typeof
b==="number")return typeof
a==="number"?c:1;else{if(0===b[0]){var
d=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
b=d,a=a[1];continue}var
c=2,b=d,a=a[1];continue}}var
e=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
c=1,b=e,a=a[1];continue}var
b=e,a=a[1];continue}}}var
mu=0;function
g5(a,b){return g4(mu,a,b)}function
ff(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(g5(e,f)){case
0:return b;case
1:var
c=d,a=b,b=fd(f,e);continue;default:var
c=d,b=fd(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,ff(d,g,a[1])]}}}return 0}}function
mv(b,a){var
c=co(a);return ff(fc(co(b),c),b,a)}function
g6(a){return a?aY(g6(a[1])):0}var
z=[0,aY,bU,cj,ck,cl,cm,g3,bV,cn,fd,fe,co,g4,g5,ff,mv,g6],mw=[0,function(b){return b?[0,a(z[17],b[1])]:0}];function
ea(a,d,b){if(typeof
b==="number")return d;else{if(0===b[0]){var
e=ea(a,d,b[1]);return c(a,d,c(a,e,e))}var
f=ea(a,d,b[1]);return c(a,f,f)}}function
g7(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
bW(c,b){if(b){var
d=b[1],e=bW(c,b[2]);return[0,a(c,d),e]}return 0}function
fg(d,b,a){if(a){var
e=a[1];return c(d,e,fg(d,b,a[2]))}return b}function
fh(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
g8(b){return typeof
b==="number"?mx:0===b[0]?[0,[0,b[1]]]:[1,a(z[4],b[1])]}function
g9(b){return typeof
b==="number"?my:0===b[0]?[0,a(z[4],b[1])]:[1,[0,b[1]]]}function
bz(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?[1,[1,b[1]]]:[1,a(z[4],b[1])];else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?[0,[1,d]]:0===b[0]?fh(bz(d,b[1])):g8(bz(d,b[1]))}var
e=c[1];return typeof
b==="number"?[0,a(z[4],e)]:0===b[0]?g9(bz(e,b[1])):fh(bz(e,b[1]))}}function
bk(b,a){if(typeof
b==="number")return a;else{if(0===b[0]){var
d=b[1];return typeof
a==="number"?b:0===a[0]?[0,c(z[2],d,a[1])]:bz(d,a[1])}var
e=b[1];return typeof
a==="number"?b:0===a[0]?bz(a[1],e):[1,c(z[2],e,a[1])]}}function
bA(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
eb(b,a){return bk(b,bA(a))}function
bB(b,a){if(typeof
b==="number")return 0;else{if(0===b[0]){var
d=b[1];return typeof
a==="number"?0:0===a[0]?[0,c(z[11],d,a[1])]:[1,c(z[11],d,a[1])]}var
e=b[1];return typeof
a==="number"?0:0===a[0]?[1,c(z[11],e,a[1])]:[0,c(z[11],e,a[1])]}}function
cp(b,a){if(typeof
b==="number")return typeof
a==="number"?0:0===a[0]?1:2;else{if(0===b[0]){var
d=b[1];if(typeof
a!=="number"&&0===a[0])return c(z[14],d,a[1]);return 2}var
e=b[1];if(typeof
a!=="number"&&1===a[0])return g2(c(z[14],e,a[1]));return 1}}function
g_(b,a){return 2<=cp(b,a)?0:1}function
fi(b,a){return 1===cp(b,a)?1:0}function
mz(b,a){return 2<=cp(b,a)?1:0}function
mA(b,a){return 1===cp(b,a)?a:b}function
ec(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
mB(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
bC(b,a){if(typeof
b==="number")return g_(mC,a)?mD:mE;else{if(0===b[0]){var
e=bC(b[1],a),f=e[1],c=bk(bB(mG,e[2]),mF);if(fi(c,a))return[0,bB(mH,f),c];var
i=eb(c,a);return[0,bk(bB(mJ,f),mI),i]}var
g=bC(b[1],a),h=g[1],d=bB(mK,g[2]);if(fi(d,a))return[0,bB(mL,h),d];var
j=eb(d,a);return[0,bk(bB(mN,h),mM),j]}}function
g$(b,a){if(typeof
b==="number")return mO;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return mP;else{if(0===a[0])return bC(c,a);var
d=bC(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,bA(f),0];var
l=bk(a,e);return[0,bA(bk(f,mQ)),l]}}var
g=b[1];if(typeof
a==="number")return mR;else{if(0===a[0]){var
h=bC(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,bA(j),0];var
m=eb(a,i);return[0,bA(bk(j,mS)),m]}var
k=bC(g,[0,a[1]]),n=k[1];return[0,n,bA(k[2])]}}}function
mT(b,a){return g$(b,a)[1]}var
s=[0,fh,g8,g9,bz,bk,bA,eb,bB,cp,g_,fi,mz,mA,ec,mB,bC,g$,mT,function(b,a){if(typeof
b==="number")return ec(a);else{if(0===b[0]){var
d=b[1];return typeof
a==="number"?ec(b):0===a[0]?[0,c(z[16],d,a[1])]:[0,c(z[16],d,a[1])]}var
e=b[1];return typeof
a==="number"?ec(b):0===a[0]?[0,c(z[16],e,a[1])]:[0,c(z[16],e,a[1])]}}];function
av(b,a){return 0===c(s[9],b,a)?1:0}function
mU(a){return[0,a]}function
mV(a){return[0,a]}function
fj(d,f,e){var
b=f,a=e;for(;;)switch(b[0]){case
0:var
g=b[1];return 0===a[0]?c(d,g,a[1]):0;case
1:var
h=b[2],i=b[1];if(1===a[0]){var
j=a[2];if(0===c(z[14],i,a[1])){var
b=h,a=j;continue}return 0}return 0;default:var
k=b[3],l=b[2],m=b[1];if(2===a[0]){var
n=a[3],o=a[1];if(0===c(z[14],l,a[2])){if(fj(d,m,o)){var
b=k,a=n;continue}return 0}return 0}return 0}}function
X(b,a){switch(a[0]){case
0:return a;case
1:var
d=a[2];return[1,c(z[2],b,a[1]),d];default:return[1,b,a]}}function
ha(b,c){return typeof
b==="number"?c:0===b[0]?[1,[1,b[1]],c]:[1,a(z[4],b[1]),c]}function
K(f,e,a,d,b){switch(a[0]){case
0:return c(e,a[1],f)?X(0,b):[2,a,d,b];case
1:return[2,a,d,b];default:var
g=a[2],h=a[1];return fj(e,a[3],[0,f])?[2,h,c(z[2],g,d),b]:[2,a,d,b]}}function
hb(c,b,a){return[2,[0,b],a,[0,c]]}function
hc(b,a){return hb(b,a,0)}function
ab(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1];return[1,d,ab(c,b[2])];default:var
e=b[2],f=b[1],g=ab(c,b[3]);return[2,ab(c,f),e,g]}}function
bl(d,a,b){switch(a[0]){case
0:return[0,c(d,a[1],b)];case
1:var
e=a[1];return[1,e,bl(d,a[2],b)];default:var
f=a[2],g=a[1];return[2,g,f,bl(d,a[3],b)]}}function
bX(d,a,b){switch(a[0]){case
0:return[0,c(d,a[1],b)];case
1:var
e=a[1];return[1,e,bX(d,a[2],b)];default:var
f=a[2],g=a[1];return[2,g,f,bX(d,a[3],b)]}}function
cq(g,f,e,b,d){switch(d[0]){case
0:return X(b,bl(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=c(s[4],m,b);return typeof
h==="number"?X(b,c(f,i,e)):0===h[0]?X(b,c(f,[1,h[1],i],e)):X(m,cq(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
b==="number"?[2,l,k,c(f,j,e)]:0===b[0]?[2,l,k,cq(g,f,e,[1,b[1]],j)]:[2,l,k,cq(g,f,e,a(z[4],b[1]),j)]}}function
cr(h,g,f,e,b,d){switch(d[0]){case
0:var
o=d[1];return X(b,bl(h,ab(g,e),o));case
1:var
j=d[2],n=d[1],i=c(s[4],n,b);return typeof
i==="number"?X(b,c(f,j,e)):0===i[0]?X(b,c(f,[1,i[1],j],e)):X(n,cr(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
b==="number"?[2,m,l,c(f,k,e)]:0===b[0]?[2,m,l,cr(h,g,f,e,[1,b[1]],k)]:[2,m,l,cr(h,g,f,e,a(z[4],b[1]),k)]}}function
fk(f,g,j,d,e,b){switch(b[0]){case
0:return[2,d,e,b];case
1:var
k=b[2],h=b[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,a(z[4],h[1]),k]];default:var
l=b[3],m=b[2],n=b[1],i=c(s[4],m,e);return typeof
i==="number"?K(f,g,c(j,n,d),m,l):0===i[0]?K(f,g,c(j,[2,n,i[1],[0,f]],d),e,l):K(f,g,fk(f,g,j,d,i[1],n),m,l)}}function
fl(g,f,h,k,d,e,b){switch(b[0]){case
0:return[2,ab(f,d),e,b];case
1:var
l=b[2],i=b[1];if(typeof
i==="number")return[2,ab(f,d),e,l];else{if(0===i[0]){var
p=[1,[1,i[1]],l];return[2,ab(f,d),e,p]}var
q=[1,a(z[4],i[1]),l];return[2,ab(f,d),e,q]}default:var
m=b[3],n=b[2],o=b[1],j=c(s[4],n,e);return typeof
j==="number"?K(g,h,c(k,o,d),n,m):0===j[0]?K(g,h,c(k,[2,o,j[1],[0,g]],d),e,m):K(g,h,fl(g,f,h,k,d,j[1],o),n,m)}}function
R(b,e,d,f,g){switch(g[0]){case
0:return bl(e,f,g[1]);case
1:var
q=g[2],r=g[1];return cq(e,function(a,c){return R(b,e,d,a,c)},q,r,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,bl(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,R(b,e,d,m,h)]:0===k[0]?[2,i,j,R(b,e,d,[1,[1,k[1]],m],h)]:[2,i,j,R(b,e,d,[1,a(z[4],k[1]),m],h)];default:var
n=f[3],o=f[2],p=f[1],l=c(s[4],o,j);if(typeof
l==="number"){var
t=R(b,e,d,n,h);return K(b,d,R(b,e,d,p,i),o,t)}else{if(0===l[0]){var
u=l[1],v=R(b,e,d,n,h);return K(b,d,R(b,e,d,[2,p,u,[0,b]],i),j,v)}var
w=l[1],x=R(b,e,d,n,h);return K(b,d,fk(b,d,function(a,c){return R(b,e,d,a,c)},i,w,p),o,x)}}}}function
E(d,f,g,b,e,h,i){switch(i[0]){case
0:return bX(g,h,i[1]);case
1:var
t=i[2],u=i[1];return cr(f,b,function(a,c){return E(d,f,g,b,e,a,c)},t,u,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],w=bl(f,ab(b,j),v);return[2,ab(b,k),l,w];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
x=E(d,f,g,b,e,o,j);return[2,ab(b,k),l,x]}else{if(0===m[0]){var
y=E(d,f,g,b,e,[1,[1,m[1]],o],j);return[2,ab(b,k),l,y]}var
A=E(d,f,g,b,e,[1,a(z[4],m[1]),o],j);return[2,ab(b,k),l,A]}default:var
p=h[3],q=h[2],r=h[1],n=c(s[4],q,l);if(typeof
n==="number"){var
B=E(d,f,g,b,e,p,j);return K(d,e,E(d,f,g,b,e,r,k),q,B)}else{if(0===n[0]){var
C=n[1],D=E(d,f,g,b,e,p,j);return K(d,e,E(d,f,g,b,e,[2,r,C,[0,d]],k),l,D)}var
F=n[1],G=E(d,f,g,b,e,p,j);return K(d,e,fl(d,b,e,function(a,c){return E(d,f,g,b,e,a,c)},k,F,r),q,G)}}}}function
cs(f,e,d,a,b){switch(a[0]){case
0:return[0,c(e,a[1],b)];case
1:var
g=a[1];return X(g,cs(f,e,d,a[2],b));default:var
h=a[2],i=a[1],j=cs(f,e,d,a[3],b);return K(f,d,cs(f,e,d,i,b),h,j)}}function
ct(d,g,f,b,e,a){return c(b,a,d)?[0,d]:c(b,a,g)?e:cs(d,f,b,e,a)}function
aZ(f,j,i,e,g,d,b,h){switch(h[0]){case
0:return X(b,ct(f,j,i,e,d,h[1]));case
1:var
l=h[2],p=h[1],k=c(s[4],p,b);return typeof
k==="number"?X(b,c(g,l,d)):0===k[0]?X(b,c(g,[1,k[1],l],d)):X(p,aZ(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
b==="number"){var
q=c(g,m,d);return K(f,e,aZ(f,j,i,e,g,d,0,o),n,q)}else{if(0===b[0]){var
r=aZ(f,j,i,e,g,d,[1,b[1]],m);return K(f,e,aZ(f,j,i,e,g,d,b,o),n,r)}var
t=aZ(f,j,i,e,g,d,a(z[4],b[1]),m);return K(f,e,aZ(f,j,i,e,g,d,b,o),n,t)}}}function
_(b,e,f,d,c,g,h){switch(h[0]){case
0:return ct(b,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return aZ(b,e,d,c,function(a,g){return _(b,e,f,d,c,a,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return ct(b,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?_(b,e,f,d,c,l,i):0===j[0]?_(b,e,f,d,c,[1,[1,j[1]],l],i):_(b,e,f,d,c,[1,a(z[4],j[1]),l],i);return K(b,c,_(b,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=_(b,e,f,d,c,n,i),u=0,v=aZ(b,e,d,c,function(a,g){return _(b,e,f,d,c,a,g)},i,u,p),w=_(b,e,f,d,c,X(0,n),k),x=_(b,e,f,d,c,p,k),y=K(b,c,v,o,t);return R(b,f,c,K(b,c,R(b,f,c,K(b,c,x,o,[0,b]),w),m,[0,b]),y)}}}function
cu(a,e,g,f,b,d){switch(d[0]){case
0:var
h=d[1];return[0,c(f,h,h)];case
1:var
l=d[1];return[1,l,cu(a,e,g,f,b,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=_(a,e,g,f,b,k,X(0,ct(a,e,f,b,i,c(g,e,e)))),n=cu(a,e,g,f,b,i);return K(a,b,R(a,g,b,K(a,b,cu(a,e,g,f,b,k),j,[0,a]),m),j,n)}}function
hd(c,b,a){return ha(a,hc(c,b))}function
cv(h,g,f,e,d,c,n,b,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return a(c,_(h,g,f,e,d,j,b));else{if(0===i[0]){var
k=i[1];return a(c,_(h,g,f,e,d,cv(h,g,f,e,d,c,cv(h,g,f,e,d,c,j,b,k),b,k),b))}var
l=i[1],j=cv(h,g,f,e,d,c,j,b,l),i=l;continue}}function
he(h,a,g,f,e,d,c,b){return b?cv(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
P(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return hd(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=P(a,f,c,g,e,d,b,j[1]);return E(a,c,e,d,b,P(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=P(a,f,c,g,e,d,b,i[1]);return E(a,c,e,d,b,P(a,f,c,g,e,d,b,j),l)}var
k=P(a,f,c,g,e,d,b,i);return R(a,c,b,P(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=P(a,f,c,g,e,d,b,h[2]);return E(a,c,e,d,b,P(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=P(a,f,c,g,e,d,b,h[2]);return _(a,f,c,g,b,P(a,f,c,g,e,d,b,p),q);case
5:return ab(d,P(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=P(a,f,c,g,e,d,b,h[1]);return he(a,f,c,g,b,function(a){return a},s,r)}}function
a0(c,b){if(typeof
b==="number")switch(b){case
0:return 0;case
1:return 1;default:return 2}else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1],e=a0(c,b[2]);return[1,a0(c,d),e];case
2:var
f=b[1],g=a0(c,b[2]);return[2,a0(c,f),g];case
3:return[3,a0(c,b[1])];default:var
h=b[1],i=a0(c,b[2]);return[4,a0(c,h),i]}}var
ed=0;function
ef(e,d,b,f){if(f){var
h=f[2],g=f[1],i=c(d,b,g);if(i){if(a(e,i[1]))return 0;var
j=ef(e,d,b,h);return j?[0,[0,g,j[1]]]:0}var
k=ef(e,d,b,h);return k?[0,[0,g,k[1]]]:0}var
l=c(d,b,b);return l?a(e,l[1])?0:[0,[0,b,0]]:[0,[0,b,0]]}function
hf(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=ef(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
hg(e,d,c,a){var
b=0;return fg(function(f,a){var
b=hf(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
cw(d,c,a,b){if(a){var
e=a[2],f=hg(d,c,a[1],b);return by(cw(d,c,e,b),f)}return ed}function
ai(d,c,f,e,p,o){var
b=p,g=o;for(;;)if(typeof
g==="number")switch(g){case
0:return b?ed:ee;case
1:return b?ee:ed;default:return ee}else
switch(g[0]){case
0:var
h=g[1];return b?a(f,h):a(e,h);case
1:var
i=g[2],j=g[1];if(b){var
q=ai(d,c,f,e,b,i);return by(ai(d,c,f,e,b,j),q)}var
r=ai(d,c,f,e,b,i);return cw(d,c,ai(d,c,f,e,b,j),r);case
2:var
k=g[2],l=g[1];if(b){var
s=ai(d,c,f,e,b,k);return cw(d,c,ai(d,c,f,e,b,l),s)}var
t=ai(d,c,f,e,b,k);return by(ai(d,c,f,e,b,l),t);case
3:var
u=g[1],b=bx(b),g=u;continue;default:var
m=g[2],n=g[1];if(b){var
v=ai(d,c,f,e,b,m);return cw(d,c,ai(d,c,f,e,bx(b),n),v)}var
w=ai(d,c,f,e,b,m);return by(ai(d,c,f,e,bx(b),n),w)}}function
hh(f,e,d){var
b=e,a=d;for(;;){if(b){var
g=b[2],h=b[1];if(a){var
i=a[2];if(c(f,h,a[1])){var
b=g,a=i;continue}return 0}return 0}return 1}}function
eg(g,f,e,d,c,b,a){return hh(c,ai(g,f,e,d,1,b),a)}function
fm(d,b,a){return bx(c(d,b,a))}function
fn(f,e,b,a){var
d=c(e,b,a);return d?fm(f,b,a):d}function
hi(b,a){switch(b){case
0:return mW;case
1:return 1===a?mX:0===a?mY:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?mZ:m0}}function
hj(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?m1:0;case
2:return 1===a?0:m2;default:return 1===a?0:0===a?m3:[0,a]}}function
eh(c,b){return b?a(c,b[1]):0}function
fo(d,b,a){if(b){var
e=b[1];return a?c(d,e,a[1]):0}return 0}function
hk(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,_(g,f,e,d,c,b,h),0]]:0}function
hl(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=hi(b[2],a[2]);return eh(function(a){return[0,[0,_(g,f,e,d,c,h,i),a]]},j)}function
cx(e,d,c,b,a){var
f=b[1],g=a[1],h=hj(b[2],a[2]);return eh(function(a){return[0,[0,R(e,d,c,f,g),a]]},h)}function
bm(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,g7(b[1],g,[0,[0,a],0])];case
1:return[0,[0,cu(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=bm(a,f,d,e,c,h,g,b[2]);return eh(function(b){return hk(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=bm(a,f,d,e,c,h,g,b[2]),n=bm(a,f,d,e,c,h,g,l);return fo(function(b,g){return hl(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=bm(a,f,d,e,c,h,g,b[2]),q=bm(a,f,d,e,c,h,g,o);return fo(function(b,e){return cx(a,d,c,b,e)},q,p);default:var
i=b[1];return fn(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
cy(a,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
b=g[1];switch(h){case
0:return fm(d,b,a);case
1:return c(d,b,a);case
2:return c(f,b,a);default:return fn(d,f,b,a)}}return 0}function
ei(c,i,h,g,b,a,f,e){var
d=bm(c,i,h,g,b,a,f,e);return d?cy(c,b,a,d[1]):0}function
hm(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=P(e,j,d,i,c,b,a,h[1]),g=P(e,j,d,i,c,b,a,k);switch(l){case
0:var
m=[0,[0,E(e,d,c,b,a,g,f),2],0];return[0,[0,E(e,d,c,b,a,f,g),2],m];case
1:return[0,[0,E(e,d,c,b,a,f,g),0],0];case
2:return[0,[0,E(e,d,c,b,a,f,g),2],0];case
3:return[0,[0,E(e,d,c,b,a,g,f),2],0];case
4:return[0,[0,E(e,d,c,b,a,f,g),3],0];default:return[0,[0,E(e,d,c,b,a,g,f),3],0]}}function
fp(h,g,f,e,d,c,b,a){var
i=hm(h,g,f,e,d,c,b,a);return bW(function(a){return[0,a,0]},i)}function
hn(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=P(e,j,d,i,c,b,a,h[1]),g=P(e,j,d,i,c,b,a,k);switch(l){case
0:return[0,[0,E(e,d,c,b,a,f,g),0],0];case
1:var
m=[0,[0,E(e,d,c,b,a,g,f),2],0];return[0,[0,E(e,d,c,b,a,f,g),2],m];case
2:return[0,[0,E(e,d,c,b,a,g,f),3],0];case
3:return[0,[0,E(e,d,c,b,a,f,g),3],0];case
4:return[0,[0,E(e,d,c,b,a,g,f),2],0];default:return[0,[0,E(e,d,c,b,a,f,g),2],0]}}function
fq(h,g,f,e,d,c,b,a){var
i=hn(h,g,f,e,d,c,b,a);return bW(function(a){return[0,a,0]},i)}function
ej(f,e){var
d=f,b=e;for(;;)switch(b[0]){case
0:return[0,b[1]];case
1:var
g=b[2],d=c(z[2],b[1],d),b=g;continue;default:var
h=b[3],i=b[2],j=b[1],k=ej(a(z[1],d),h);return[2,[4,ej(d,j),[6,[1,d],[0,i]]],k]}}function
m4(a){return ej(0,a)}function
aw(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1]];case
2:var
d=b[1],e=aw(c,b[2]);return[2,aw(c,d),e];case
3:var
f=b[1],g=aw(c,b[2]);return[3,aw(c,f),g];case
4:var
h=b[1],i=aw(c,b[2]);return[4,aw(c,h),i];case
5:return[5,aw(c,b[1])];default:var
j=b[2];return[6,aw(c,b[1]),j]}}function
ho(b,a){var
c=a[2],d=a[1],e=aw(b,a[3]);return[0,aw(b,d),c,e]}function
m5(q,h,f,g,b){if(typeof
b!=="number")switch(b[0]){case
1:var
m=b[1];if(0===m[0]){var
n=m[1];return c(g,q,n)?0:[5,c(f,n,n)]}return[1,m];case
3:var
a=b[2],d=b[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
a==="number"?0:5===a[0]?[3,[5,c(f,a[1],s)],i]:b}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
a==="number"?0:5===a[0]?[3,[5,c(f,a[1],r)],j]:b}return typeof
a==="number"?0:5===a[0]?c(g,h,a[1])?d:[3,d,a]:b;case
5:var
e=d[1];if(typeof
a==="number")return 0;else
switch(a[0]){case
3:var
k=a[2],l=a[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,c(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,c(f,e,k[1])],l];return c(g,h,e)?a:[3,d,a];case
4:return[4,[3,[5,e],a[1]],[3,[5,e],a[2]]];case
5:return[5,c(f,e,a[1])];default:return c(g,h,e)?a:[3,d,a]}default:return typeof
a==="number"?0:5===a[0]?c(g,h,a[1])?d:[3,d,a]:b}case
4:var
o=b[2],p=b[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return b}function
m6(a){return a[1]}function
m7(a){return a[2]}function
ax(b,a){var
d=c(s[8],a[1],[0,b[2]]);return av(c(s[8],b[1],[0,a[2]]),d)}function
cz(b,a){var
d=c(s[8],a[1],[0,b[2]]),e=c(s[8],b[1],[0,a[2]]);return c(s[10],e,d)}function
ay(b,a){var
d=c(z[11],b[2],a[2]),e=c(s[8],a[1],[0,b[2]]),f=c(s[8],b[1],[0,a[2]]);return[0,c(s[5],f,e),d]}function
a1(b,a){var
d=c(z[11],b[2],a[2]);return[0,c(s[8],b[1],a[1]),d]}function
bD(b){var
c=b[2];return[0,a(s[6],b[1]),c]}function
bY(b,a){return ay(b,bD(a))}function
fr(b){var
a=b[1];return typeof
a==="number"?m8:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
fs(a,b){return ea(a1,a,b)}function
m9(b,a){return typeof
a==="number"?m_:0===a[0]?fs(b,a[1]):fr(fs(b,a[1]))}function
m$(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
bZ(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,bZ(b,a[1],c)]:[1,bZ(b,a[1],c),b,0]}function
ft(d,a,b,c){if(typeof
c==="number")return bZ(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,bZ(d,a[1],b)]:[1,bZ(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,ft(d,a[1],b,e)]:[1,ft(d,a[1],b,f),h,e]}}var
na=s[10],nb=s[8],nc=s[5],ne=0;function
nf(a,b){return ei(ne,nd,nc,nb,av,na,a,b)}var
ng=s[6],nh=s[7],ni=s[5],nj=0;function
$(a,b){return E(nj,ni,nh,ng,av,a,b)}var
nk=s[5],nl=0;function
az(a,b){return R(nl,nk,av,a,b)}var
nm=s[6],nn=s[7],no=s[8],np=s[5],nr=0;function
cA(a){return P(nr,nq,np,no,nn,nm,av,a)}function
hp(c){var
d=c[3],e=c[2],a=cA(c[1]),b=cA(d);switch(e){case
0:var
f=[0,[0,$(b,az(a,ns)),3],0];return[0,[0,$(a,az(b,nt)),3],f];case
1:return[0,[0,$(a,b),0],0];case
2:return[0,[0,$(a,az(b,nu)),3],0];case
3:return[0,[0,$(b,az(a,nv)),3],0];case
4:return[0,[0,$(a,b),3],0];default:return[0,[0,$(b,a),3],0]}}function
hq(a){var
b=hp(a);return bW(function(a){return[0,a,0]},b)}function
hr(c){var
d=c[3],e=c[2],a=cA(c[1]),b=cA(d);switch(e){case
0:return[0,[0,$(a,b),0],0];case
1:var
f=[0,[0,$(b,az(a,nw)),3],0];return[0,[0,$(a,az(b,nx)),3],f];case
2:return[0,[0,$(b,a),3],0];case
3:return[0,[0,$(a,b),3],0];case
4:return[0,[0,$(b,az(a,ny)),3],0];default:return[0,[0,$(a,az(b,nz)),3],0]}}function
hs(a){var
b=hr(a);return bW(function(a){return[0,a,0]},b)}var
nA=s[10],nB=0;function
fu(a){return cy(nB,av,nA,a)}var
nC=s[5],nD=0;function
ht(a,b){return cx(nD,nC,av,a,b)}function
hu(e,d){var
a=c(s[17],e,d),b=a[1];return typeof
a[2]==="number"?b:c(s[5],b,nE)}function
fv(b,a){var
d=c(s[19],b,a);return c(s[13],d,nF)}function
cB(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=cB(a[1]),f=b[2],g=b[1],c=cB(e),h=c[2],i=c[1];return[0,fv(fv(g,f),i),h]}}function
cC(a,b){switch(a[0]){case
0:return[0,c(s[18],a[1],b)];case
1:var
d=a[1];return[1,d,cC(a[2],b)];default:var
e=a[2],f=a[1],g=cC(a[3],b);return[2,cC(f,b),e,g]}}function
ek(b){var
e=cB(b),f=e[2],d=e[1];if(c(s[12],d,0)){var
g=hu(a(s[6],f),d),h=a(s[6],g);return[0,cC(bX(s[7],b,f),d),h]}return[0,b,0]}function
el(d){var
e=d[2],a=d[1];switch(e){case
0:var
f=cB(a),g=f[2],b=f[1];if(c(s[12],b,0))if(bx(av(g,0)))if(bx(av(c(s[19],b,g),b)))return 0;return[0,[0,ek(a),0]];case
1:return[0,[0,[0,a,0],e]];case
2:return[0,[0,ek(bX(s[7],a,nG)),3]];default:return[0,[0,ek(a),3]]}}function
hv(a){var
b=a[1],c=a[2];return[0,az(b[1],[0,b[2]]),c]}function
hw(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
nH=s[10],nI=s[8],nJ=s[5],nL=0;function
cD(a,b){return bm(nL,nK,nJ,nI,av,nH,a,b)}function
fw(a){return 0===a?1:3<=a?1:0}function
fx(w,v){var
d=w,b=v;for(;;)if(typeof
b==="number")return 0;else
switch(b[0]){case
0:var
x=b[2],g=cD(d,b[1]);if(g){var
h=g[1];if(fu(h))return 1;var
d=[0,h,d],b=x;continue}return 0;case
1:var
y=b[2],i=cD(d,b[1]);if(i){var
j=el(i[1]);if(j){var
d=[0,hv(j[1]),d],b=y;continue}return 1}return 0;default:var
z=b[3],A=b[2],k=cD(d,b[1]);if(k){var
B=k[1],l=cD(d,A);if(l){var
C=l[1],m=el(B);if(m){var
n=m[1],o=n[1],p=o[1],D=n[2],E=o[2],q=el(C);if(q){var
r=q[1],t=r[1],F=r[2],G=t[2],H=t[1];if(fw(D))if(fw(F))if(hw(az(p,H))){var
f=z,e=a(s[6],E);for(;;){if(f){var
I=f[2],J=f[1],u=fx([0,[0,$(p,[0,e]),0],d],J);if(u){var
f=I,e=c(s[5],e,nM);continue}return u}return c(s[12],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
nN(b,a){return eg(fu,ht,hq,hs,fx,b,a)}function
hx(a,b){return ei(nP,nO,ay,a1,ax,cz,a,b)}function
hy(a){return fp(nR,nQ,ay,a1,bY,bD,ax,a)}function
hz(a){return fq(nT,nS,ay,a1,bY,bD,ax,a)}function
hA(a){return cy(nU,ax,cz,a)}function
hB(a,b){return cx(nV,ay,ax,a,b)}function
nW(b,a){return eg(hA,hB,hy,hz,hx,b,a)}function
aF(a){if(typeof
a==="number")return 0===a?nX:nY;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aF(a[2]);return ay(aF(b),c);case
3:var
d=a[1],e=aF(a[2]);return bY(aF(d),e);case
4:var
f=a[1],g=aF(a[2]);return a1(aF(f),g);case
5:return fr(aF(a[1]));default:return bD(aF(a[1]))}}function
hC(a,b){return ei(n0,nZ,ay,a1,ax,cz,a,b)}function
hD(a){return fp(n2,n1,ay,a1,bY,bD,ax,a)}function
hE(a){return fq(n4,n3,ay,a1,bY,bD,ax,a)}function
hF(a){return cy(n5,ax,cz,a)}function
hG(a,b){return cx(n6,ay,ax,a,b)}var
r=[0,bx,by,g2,fc,mo,z,mw,ea,g7,bW,fg,s,av,mU,mV,fj,X,ha,K,hb,hc,ab,bl,bX,cq,cr,fk,fl,R,E,cs,ct,aZ,_,cu,hd,cv,he,P,a0,ed,ee,ef,hf,hg,cw,by,ai,hh,eg,fm,fn,hi,hj,eh,fo,hk,hl,cx,bm,cy,ei,P,E,R,hm,fp,hn,fq,ej,m4,aw,ho,m5,m6,m7,ax,cz,ay,a1,bD,bY,fr,fs,m9,m$,bZ,ft,nf,$,az,cA,hp,hq,hr,hs,fu,ht,hu,fv,cB,cC,ek,el,hv,hw,cD,fw,fx,nN,hx,hy,hz,hA,hB,nW,aF,hC,hD,hE,hF,hG,function(b,a){return eg(hF,hG,hD,hE,hC,a0(function(a){return ho(aF,a)},b),a)}];aU(692,r,"Micromega_plugin.Micromega");var
fy=f[2],bE=f[7],n8=f[3],Q=a(bF[1],[0,Z]),n7=0;function
n9(a,b){function
d(c,b){return 1===b?e(n[1],a,n_,c):x(n[1],a,n$,c,b)}return c(Q[12],d,b)}var
fz=Q[1];function
hH(a){var
b=0;function
c(c,b,a){return a+b|0}return e(Q[13],c,a,b)}function
oa(b,a){var
c=hH(b),d=hH(a);return c===d?e(Q[10],Z,b,a):Z(c,d)}function
hI(a){return V(a,Q[1])}function
ob(a){return e(Q[4],a,1,Q[1])}function
oc(a){try{var
b=1,c=function(c,b,a){if(1===a){if(1===b)return 0;throw M}throw M},d=1-e(Q[13],c,a,b);return d}catch(a){a=v(a);if(a===M)return 0;throw a}}function
od(a){if(hI(a))return 0;try{var
b=function(c,b,f){var
d=b/2|0;if(0===(b%2|0))return e(Q[4],c,d,a);throw M},c=[0,e(Q[13],b,a,fz)];return c}catch(a){a=v(a);if(a===M)return 0;throw a}}function
cE(b,a){try{var
d=c(Q[27],b,a);return d}catch(a){a=v(a);if(a===M)return 0;throw a}}function
oe(b,a){var
c=cE(b,a)+1|0;return e(Q[4],b,c,a)}function
hJ(b,a){function
c(b,c,a){var
d=cE(b,a)+c|0;return e(Q[4],b,d,a)}return e(Q[13],c,b,a)}function
of(d,c){var
b=fz,a=c;for(;;){if(0===a)return b;var
b=hJ(b,d),a=a-1|0;continue}}function
og(b,a){var
f=l[7];function
g(e,d,a){var
f=G.caml_div(cE(e,b),d);return c(l[4],a,f)}var
d=e(Q[13],g,a,f),h=Q[1];function
i(c,g,b){var
f=g-eW(cE(c,a),d)|0;return 0===f?b:e(Q[4],c,f,b)}return[0,e(Q[13],i,b,h),d]}var
H=[0,fz,hI,ob,oc,cE,oe,hJ,of,og,oa,n9,Q[13],od],S=a(bF[1],[0,H[10]]);function
oh(b,d){function
g(g,d){if(0===c(H[10],H[1],g)){var
h=a(f[40],d);return e(n[1],b,oi,h)}var
i=H[11],j=a(f[40],d);return aT(n[1],b,oj,j,i,g)}return c(S[12],g,d)}function
hK(b,a){try{var
d=c(S[27],b,a);return d}catch(a){a=v(a);if(a===M)return ok;throw a}}function
ol(b){var
c=S[1],d=a(H[3],b);return e(S[4],d,om,c)}function
hL(a){return e(S[4],H[1],a,S[1])}function
hM(d,g,b){if(0===a(f[25],g))return b;var
h=c(fy,hK(d,b),g);return 0===a(f[25],h)?c(S[7],d,b):e(S[4],d,h,b)}function
hN(g,b,d){if(0===a(f[25],b))return hL(on);var
h=S[1];function
i(f,d,a){var
h=c(bE,b,d),i=c(H[7],g,f);return e(S[4],i,h,a)}return e(S[13],i,d,h)}function
hO(b,a){function
c(c,b,a){return hM(c,b,a)}return e(S[13],c,b,a)}function
oo(b,a){var
c=S[1];function
d(d,c,b){return hO(hN(d,c,a),b)}return e(S[13],d,b,c)}function
op(b){function
d(b){return a(f[3],b)}return c(S[33],d,b)}var
hP=S[13];function
oq(b){var
c=1;return e(hP,function(e,c,b){var
d=b?0===a(f[25],c)?1:0:b;return d},b,c)}var
or=a(S[10],f[37]),hQ=[0,hK,ol,hM,hL,hN,oo,hO,op,hP,oh,or,oq,function(b){var
c=1;function
d(c,f,b){if(b){var
d=a(H[2],c);if(!d)return a(H[4],c);var
e=d}else
var
e=b;return e}return e(S[13],d,b,c)}];function
os(k,j){var
b=k,a=j;for(;;){if(b){var
d=b[1],l=b[2],m=d[2],n=d[1];if(a){var
e=a[1],g=n===e[1]?1:0,o=a[2],p=e[2];if(g){var
h=c(f[26],m,p);if(h){var
b=l,a=o;continue}var
i=h}else
var
i=g;return i}return 0}return a?0:1}}function
ot(e){var
c=0,b=e;for(;;){if(b){var
d=b[1],g=b[2],h=d[1],i=[0,h,a(f[56],d[2])],c=c+a(bj[21],i)|0,b=g;continue}return a(bj[21],c)}}var
ou=0;function
ov(h,b){function
d(b){var
c=b[1],d=a(f[40],b[2]);return e(n[2],ow,d,c)}return c(g[15],d,b)}function
ox(a){function
d(i,h){var
b=i,a=h;for(;;){if(a){var
e=a[2],g=a[1];if(c(f[31],g,oy))return[0,[0,b,g],d(b+1|0,e)];var
b=b+1|0,a=e;continue}return 0}}return d(0,a)}function
oA(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,em,b(c+1|0,a)]}return 0}return b(0,a)}function
bG(d,b,a){return c(f[26],b,oB)?a:[0,[0,d,b],a]}function
hR(d,c,b){if(b){var
f=b[2],g=b[1],h=g[2],e=g[1],i=Z(d,e)+1|0;if(2<i>>>0)return a(l[2],oC);switch(i){case
0:return bG(d,a(c,em),b);case
1:return bG(e,a(c,h),f);default:return[0,[0,e,h],hR(d,c,f)]}}return bG(d,a(c,em),0)}function
hS(d,c,b){if(b){var
f=b[2],g=b[1],e=g[1],h=Z(d,e)+1|0,i=g[2];if(2<h>>>0)return a(l[2],oD);switch(h){case
0:return bG(d,c,b);case
1:return bG(e,c,f);default:return[0,[0,e,i],hS(d,c,f)]}}return bG(d,c,0)}function
oE(d){var
f=q[1];function
h(d,b){var
e=a(k[18],b[2]);return c(q[17],d,e)}var
b=e(g[20],h,f,d);return 0===c(q[23],b,q[1])?q[2]:b}function
oF(a,b){if(0===a[0]){var
d=a[1];if(0===d)return 0;if(1===d)return b}function
e(b){var
d=b[1];return[0,d,c(f[7],a,b[2])]}return c(g[17],e,b)}function
en(o,n){var
b=o,a=n;for(;;){if(b){if(a){var
e=a[2],i=a[1],j=i[2],g=i[1],h=b[2],k=b[1],l=k[2],d=k[1];if(V(d,g)){var
m=c(f[1],l,j);if(c(f[26],m,oG)){var
b=h,a=e;continue}return[0,[0,d,m],en(h,e)]}return gB(d,g)?[0,[0,d,l],en(h,a)]:[0,[0,g,j],en(b,e)]}return b}return a?a:0}}function
oH(d,b){var
e=0,g=[0,function(a){return c(f[37],d[2],b[2])},e],h=[0,function(a){return Z(d[1],b[1])},g];return a(k[31][1],h)}var
oI=a(k[31][2],oH);function
hT(e,d){var
a=d;for(;;){if(a){var
b=a[1],c=Z(b[1],e),f=a[2],g=b[2];if(-1===c){var
a=f;continue}return 0===c?[0,[0,g,a]]:0}return 0}}function
oJ(c,b){var
a=hT(c,b);return a?[0,a[1][1]]:0}var
bH=[0,os,ot,ou,ov,ox,em,oz,oA,bG,hR,hS,oE,oF,en,oI,hT,oJ,function(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}];function
fA(a){return 0===a?oK:oL}function
oM(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fA(d);return C(n[1],c,oN,bH[4],e,h,g)}function
oO(b,a){if(0===b){if(0===a)return 0}else
if(0!==a)return 1;return 1}function
oP(b,a){if(0!==b)if(0!==a)return 1;return 0}function
aA(d,b){if(typeof
b==="number")return c(n[1],d,oQ);else
switch(b[0]){case
0:return e(n[1],d,oR,b[1]);case
1:return e(n[1],d,oS,b[1]);case
2:var
f=a(q[33],b[1]);return e(n[1],d,oT,f);case
3:return c(n[1],d,oU);case
4:return x(n[1],d,oV,aA,b[2]);case
5:var
g=b[2],h=a(q[33],b[1]);return aT(n[1],d,oW,aA,g,h);case
6:return C(n[1],d,oX,aA,b[1],aA,b[2]);case
7:return C(n[1],d,oY,aA,b[1],aA,b[2]);default:return x(n[1],d,oZ,aA,b[1])}}function
cF(d,b){if(typeof
b==="number")return c(n[1],d,o0);else{if(0===b[0])return d0(n[1],d,o1,b[1],aA,b[2],cF,b[3]);var
e=b[5],f=b[4],g=b[3],h=b[2],i=b[1],j=a(k[2],cF);return A7(n[1],d,o2,i,aA,h,bH[4],g,aA,f,j,e)}}function
b0(e){var
a=e;for(;;){if(typeof
a==="number")var
b=0;else
switch(a[0]){case
8:var
d=a[1],b=1;break;case
0:case
1:return a[1];case
4:case
5:var
d=a[2],b=1;break;case
6:case
7:var
f=a[1],g=b0(a[2]),h=b0(f);return c(l[5],h,g);default:var
b=0}if(b){var
a=d;continue}return-1}}function
fB(a){if(typeof
a==="number")return-1;else{if(0===a[0]){var
b=a[2],d=a[1],f=fB(a[3]),h=b0(b),i=c(l[5],h,f);return c(l[5],d,i)}var
j=a[5],k=a[2],m=a[1],n=b0(a[4]),o=b0(k),p=c(l[5],o,n),q=c(l[5],m,p),r=function(b,a){var
d=fB(a);return c(l[5],b,d)};return e(g[20],r,q,j)}}function
a2(b,a){if(typeof
a!=="number")switch(a[0]){case
4:var
n=a[1],d=a2(b,a[2]);return[0,d[1],d[2],[4,n,d[3]]];case
5:var
e=a2(b,a[2]),f=e[2];return[0,[0,[0,f,e[3]],e[1]],f+1|0,[1,f]];case
6:var
o=a[2],g=a2(b,a[1]),p=g[3],q=g[1],h=a2(g[2],o),r=h[2],s=[6,p,h[3]];return[0,c(l[25],h[1],q),r,s];case
7:var
t=a[2],i=a2(b,a[1]),u=i[3],v=i[1],j=a2(i[2],t),w=j[2],x=[7,u,j[3]];return[0,c(l[25],j[1],v),w,x];case
8:var
k=a2(b,a[1]),m=k[2];return[0,[0,[0,m,k[3]],k[1]],m+1|0,[1,m]]}return[0,0,b,a]}function
eo(c,a){if(typeof
a!=="number"&&8===a[0]){var
b=a2(c,a[1]);return[0,b[1],b[2],[8,b[3]]]}return a2(c,a)}function
fC(b){var
a=b;for(;;){if(typeof
a!=="number"&&8===a[0]){var
a=a[1];continue}return a}}function
fD(f,o){var
b=o;for(;;)if(typeof
b==="number")return[0,f,0];else{if(0===b[0]){var
d=b[2],k=b[1];if(typeof
d!=="number"&&5===d[0])if(typeof
b[3]==="number"){var
b=[0,k,d[2],0];continue}var
p=b[3],h=eo(f,d),q=h[3],r=h[1],m=fD(h[2],p),s=m[1],t=[0,k,q,m[2]],u=function(b,a){return[0,a[1],[8,a[2]],b]};return[0,s,e(g[20],u,t,r)]}var
v=b[5],w=b[4],x=b[3],y=b[1],i=eo(f,fC(b[2])),z=i[3],A=i[2],B=i[1],j=eo(A,fC(w)),C=j[3],D=j[2],E=j[1],F=function(a){return fD(D,a)},G=c(g[17],F,v),n=a(g[46],G),H=n[2],I=n[1],J=c(l[25],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[8,a[2]],b]},M=e(g[20],L,K,J);return[0,e(g[20],l[5],0,I),M]}}function
o3(c,a){var
b=fD(c,a);if(k[1])aT(n[2],o4,cF,a,cF,b[2]);return b}function
hU(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[7,b,a];var
c=b}return c}function
fE(b,d){var
e=a(q[22],b)+1|0;if(2<e>>>0)throw[0,am,o5];switch(e){case
0:return[4,[0,0,[1,b]],d];case
1:return 0;default:return c(q[24],b,q[2])?d:[6,[2,b],d]}}function
hV(c,b){var
d=c[2],e=c[1];return e?[4,[0,e,d],b]:fE(a(k[18],d),b)}var
cG=a(bF[1],[0,H[10]]),cH=a(bF[1],[0,Z]),cI=[0,cG[1]],cJ=[0,cH[1]],ep=[0,0];function
o6(a){cI[1]=cG[1];cJ[1]=cH[1];ep[1]=0;return 0}function
o7(b){try{var
a=c(cG[27],b,cI[1]);return a}catch(a){a=v(a);if(a===M){var
d=ep[1];cI[1]=e(cG[4],b,d,cI[1]);cJ[1]=e(cH[4],d,b,cJ[1]);ep[1]++;return d}throw a}}var
aG=[0,cG,cH,cI,cJ,ep,o6,o7,function(a){return c(cH[27],a,cJ[1])}];function
fF(a){var
b=a[2],d=a[1];function
e(b,a){return Z(b[1],a[1])}return[0,c(g[48],e,d),b]}function
eq(c,b){var
d=b[2],e=a(aG[8],b[1]),g=H[11],h=a(f[40],d);return aT(n[1],c,o8,h,g,e)}function
er(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=fA(d),i=a(k[2],eq);return C(n[1],c,o9,i,e,h,g)}function
o_(c){function
d(d,c,b){var
e=b[1],f=b[2];return a(H[2],d)?[0,e,c]:[0,[0,[0,a(aG[7],d),c],e],f]}var
b=e(hQ[9],d,c,o$);return fF([0,b[1],b[2]])}function
hW(b,d,m){var
e=m[2],i=m[1];if(a(H[2],d))var
q=c(bE,b,e),h=[0,c(bH[13],b,i),q];else
if(0===a(f[25],b))var
h=pa;else{if(0===a(f[25],e))var
j=0;else
var
t=c(bE,b,e),j=[0,[0,a(aG[7],d),t],0];var
r=function(e){var
f=e[2],g=a(aG[8],e[1]),h=c(H[7],d,g),i=a(aG[7],h);return[0,i,c(bE,b,f)]},s=c(g[17],r,i),h=fF([0,c(l[25],j,s),pb])}var
o=h[2],p=h[1];if(k[1]){var
u=a(f[40],o),v=a(k[2],eq),w=a(f[40],e),x=a(k[2],eq),y=H[11],z=a(f[40],b);A8(n[2],pc,z,y,d,x,i,w,v,p,u)}return[0,p,o]}function
hX(c,b){return a(H[2],b)?[0,0,c]:[0,[0,[0,a(aG[7],b),c],0],pd]}function
hY(w,v,d,u){var
h=u[1],i=w[1],F=u[2],G=w[2];if(k[1]){var
I=a(f[40],d),J=a(aG[8],v);bO(n[2],pe,er,i,H[11],J,I,er,h)}var
K=h[2],x=a(aG[8],v);function
C(b,a){var
d=a[2],e=b[2],f=a[1],g=b[1];return gB(e,d)?-1:V(e,d)?c(H[10],g,f):1}var
p=[0,H[1],l[7]],o=[0,h,F];for(;;){var
q=o[2],b=o[1],_=p[2],$=p[1],U=b[1],W=[0,pg,H[1],0],X=function(b,d){var
e=b[3],f=b[2],j=d[2],k=b[1],l=a(aG[8],d[1]),g=c(H[9],l,x),h=g[2],i=g[1];return-1===C([0,f,e],[0,i,h])?[0,j,i,h]:[0,k,f,e]},m=e(g[20],X,W,U),D=m[3],Y=m[2],Z=m[1],E=0<D?[0,[0,Z,Y,D]]:0;if(E){var
r=E[1],s=r[3],t=r[2],aa=r[1];if(-1===C([0,t,s],[0,$,_])){var
j=a(f[15],d),y=c(bE,[0,-a(f[25],d)|0],aa),L=c(H[8],x,s-1|0),z=c(H[7],t,L),M=a(f[3],i[3]),A=hW(y,z,[0,i[1],M]),N=A[2],O=A[1],P=c(bE,j,b[3]),Q=c(fy,a(f[3],N),P),R=c(bH[13],j,b[1]),B=[0,c(bH[14],R,O),K,Q],S=fE(a(k[18],j),q),T=hU(hV(hX(y,z),G),S);if(k[1])e(n[2],pf,er,B);var
p=[0,t,s],o=[0,B,T];continue}return[0,b,q]}return[0,b,q]}}var
p=[0,n7,fy,n8,bE,H,hQ,bH,fA,oM,oO,oP,aA,cF,b0,fB,eo,fC,o3,hU,fE,hV,[0,aG,fF,eq,er,o_,hW,hX,hY,function(b,a){var
d=a[1],f=a[2],e=c(bH[17],b,d[1]);if(e){var
g=e[1];return function(a){return[0,hY([0,d,f],b,g,a)]}}return function(a){return 0}}]];aU(696,p,"Micromega_plugin.Polynomial");var
an=k[4],es=k[5],ph=0,pi=0,pj=j1;function
hZ(a){var
b=a[1];if(b){var
d=a[2];if(d)return c(f[29],b[1],d[1])?[0,a]:0}return[0,a]}function
pk(a){var
b=a[2],d=c(an,f[3],a[1]);return[0,c(an,f[3],b),d]}function
pl(b,a){var
e=a[2],g=a[1],h=b[2],i=b[1];function
d(d,b,a){if(b){var
e=b[1];return a?[0,c(d,e,a[1])]:b}return a?a:0}var
j=d(f[39],h,e);return hZ([0,d(f[38],i,g),j])}function
fG(b){var
d=b[1];if(d){var
e=b[2];if(e){var
g=e[1],h=a(f[24],d[1]),i=a(f[22],g),j=c(f[4],i,h);return[0,c(f[1],j,pm)]}}return 0}function
pn(e,d){var
a=fG(e),b=fG(d);return a?b?c(f[29],a[1],b[1]):1:0}var
b1=[0,hZ,pk,pl,fG,pn,function(d,a){var
b=d[2],e=d[1];if(e){var
g=e[1];if(b){var
i=b[1],h=c(f[29],g,a);return h?c(f[29],a,i):h}return c(f[29],g,a)}return b?c(f[29],a,b[1]):1}],aH=a(d9[1],[0,Z]),h0=p[7],aa=a(bj[19],[0,h0[1],h0[2]]),h1=[0,l[7]],b2=[aW,pp,aV(0)],po=0;function
pq(a){function
d(h,g){var
a=h,b=g;for(;;){switch(a[0]){case
0:return c(aH[4],a[1],b);case
1:var
f=a[3],e=a[2];break;default:var
f=a[2],e=a[1]}var
a=e,b=d(f,b);continue}}return d(a,aH[1])}function
cK(b,a){switch(a[0]){case
0:return e(n[1],b,pr,a[1]);case
1:return d0(n[1],b,ps,a[1],cK,a[2],cK,a[3]);default:return C(n[1],b,pt,cK,a[1],cK,a[2])}}function
fH(d,b){if(b){var
e=a(f[40],b[1]);return c(l[54],d,e)}return c(l[54],d,pu)}function
h2(b,a){return C(n[1],b,pv,fH,a[1],fH,a[2])}function
pw(a,b){c(l[54],a,px);var
d=0;function
f(b,c){return e(n[1],a,py,b)}e(aH[15],f,b,d);return c(l[54],a,pz)}function
pA(a,b){c(l[54],a,pB);var
d=0;function
f(b,c){return e(n[1],a,pC,b)}e(aH[15],f,b,d);return c(l[54],a,pD)}function
pE(b,a){return h2(b,a[1])}function
h3(b,d){var
g=d[2],h=g[2],i=g[1],j=d[1];if(i){var
k=a(f[40],i[1]);e(n[1],b,pF,k)}c(p[7][4],b,j);if(h){var
m=a(f[40],h[1]);return e(n[1],b,pG,m)}return c(l[54],b,pH)}function
pI(b,a){function
d(c,a){return h3(b,[0,c,a[1][1]])}return c(aa[12],d,a)}function
pJ(c,b){var
d=b[2],e=b[1],g=a(f[40],b[3]),h=p[7][4],i=a(f[40],e);return C(n[1],c,pK,i,h,d,g)}function
h4(b,a){var
d=b[4],e=b[3],g=a[4],h=a[2],i=a[1],j=b[2],k=b[1];if(e===a[3])if(d===g){var
f=c(b1[3],k,i);return f?[0,[0,f[1],[2,j,h],e,d]]:0}throw[0,am,pL]}function
pM(f,b,d){try{var
a=c(aa[7],d,f),g=h4(b,a[1]);if(g){a[1]=g[1];var
h=0;return h}throw[0,b2,[2,b[2],a[1][2]]]}catch(a){a=v(a);if(a===M)return e(aa[10],d,f,[0,b]);throw a}}var
h5=[aW,pN,aV(0)];function
et(d,c,b){var
e=h1[1];if(a(aa[15],b)<e)return pM(d,c,b);throw h5}function
eu(d,b){var
k=a(b1[1],b[1]);if(k){var
l=k[1],i=l[2],j=l[1];if(d){var
e=d[1][2],h=function(a){return c(f[9],a,e)};if(1===a(f[25],e))var
o=b[4],p=b[3],q=b[2],r=c(an,h,i),m=[0,[0,c(an,h,j),r],q,p,o];else
var
t=b[3],u=b[4],v=b[2],w=c(an,h,j),m=[0,[0,c(an,h,i),w],v,u,t];if(c(f[31],e,pO))var
s=function(a){var
b=a[1];return[0,b,c(f[9],a[2],e)]},n=c(g[17],s,d);else
var
n=d;return[0,n,m]}return c(b1[6],[0,j,i],pP)?1:0}return 0}function
pQ(a){return 0===a?f[26]:f[30]}function
fI(h){var
d=0,c=0,b=h;for(;;){if(b){var
e=b[2],g=a(f[25],b[1][2]);if(0===g)throw[0,am,pR];if(1===g){var
c=c+1|0,b=e;continue}var
d=d+1|0,b=e;continue}return[0,d,c]}}function
fJ(a,e){var
b=a[3],c=a[1],f=a[2],d=fI(c),g=d[2],h=d[1],i=[0,e],j=0===f?[0,[0,b],[0,b]]:[0,[0,b],0];return eu(c,[0,j,i,g,h])}function
h6(d){var
b=a(aa[1],1e3);function
f(b,a){return[0,b,a]}var
h=c(k[23],f,d),i=aH[1];function
j(f,d){var
h=d[2],i=d[1],a=fJ(i,h);if(typeof
a==="number"){if(0===a)throw[0,b2,[0,h]];return f}et(a[1],a[2],b);var
j=i[1];function
k(b,a){return c(aH[4],a[1],b)}return e(g[20],k,f,j)}return[0,b,e(g[20],j,i,h)]}function
fK(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return e(aa[14],d,b,c)}function
ev(e,b){var
h=b[2],i=e[2],j=b[1],k=e[1];if(c(f[31],i,pS))if(c(f[31],h,pT)){var
d=function(s,r){var
b=s,a=r;for(;;){if(b){if(a){var
j=a[2],m=a[1],n=m[2],k=m[1],l=b[2],o=b[1],p=o[2],e=o[1];if(e===k){var
t=c(f[9],n,h),u=c(f[9],p,i),q=c(f[1],u,t);if(c(f[26],q,pU)){var
b=l,a=j;continue}return[0,[0,e,q],d(l,j)]}if(e<k){var
v=d(l,a);return[0,[0,e,c(f[9],p,i)],v]}var
w=d(b,j);return[0,[0,k,c(f[9],n,h)],w]}var
x=function(a){var
b=a[1];return[0,b,c(f[9],a[2],i)]};return c(g[17],x,b)}if(a){var
y=function(a){var
b=a[1];return[0,b,c(f[9],a[2],h)]};return c(g[17],y,a)}return 0}},a=d(k,j);return[0,a,fI(a)]}throw[0,am,pV]}function
h7(q,g,b,e){var
h=e[3],i=e[2],j=e[1],k=c(p[7][17],q,g);if(k){var
l=k[1],d=function(c,a){return a?[0,[0,l,g,[0,[0,[0,a[1]],0],b[2],b[3],b[4]]],c]:c},m=b[1],n=m[2],o=m[1];if(1===a(f[25],l)){var
r=d(h,n);return[0,d(j,o),i,r]}var
s=d(h,o);return[0,d(j,n),i,s]}return[0,j,[0,[0,g,b],i],h]}function
h8(d,b){var
j=b[1];function
k(c,b,a){return h7(d,c,b[1],a)}var
h=e(aa[14],k,j,pW),l=h[3],m=h[2],n=h[1],o=a(aa[15],b[1]),i=a(aa[1],o);function
p(a){return e(aa[10],i,a[1],[0,a[2]])}c(g[15],p,m);function
q(e){function
b(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=a(es,k[1][1]),t=a(es,r[1]),u=a(f[3],j),v=c(f[9],t,u),w=c(f[9],s,l),x=c(f[1],w,v),m=ev([0,q,l],[0,p,a(f[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,d,k[2],h[2]],n[2],n[1]],b=eu(m[1],o);if(typeof
b==="number"){if(0===b)throw[0,b2,o[2]];return 0}return et(b[1],b[2],i)}return c(g[15],b,l)}c(g[15],q,n);return[0,i,c(aH[6],d,b[2])]}function
h9(h,g,t,s,r,e){var
b=e[2],d=e[1],i=c(p[7][17],h,d);if(i){var
j=i[1],k=c(f[30],j,pX)?a(f[3],g):g,l=a(f[15],j),m=ev([0,t,k],[0,d,l]),n=m[2],u=n[2],v=n[1],w=m[1],x=c(f[9],s,k),o=function(a){var
b=c(f[9],a,l);return c(f[1],x,b)},q=b[1],y=q[1],z=c(an,o,q[2]),A=[0,c(an,o,y),z];return[0,w,[0,A,[1,h,r,b[2]],u,v]]}return[0,d,b]}function
h_(d,e,h,g,b){var
i=a(es,c(p[7][17],d,e)),j=a(aa[15],b[1]),f=a(aa[1],j),k=b[1];function
l(k,j){var
c=h9(d,i,e,h,g,[0,k,j[1]]),b=c[2],a=eu(c[1],b);if(typeof
a==="number"){if(0===a)throw[0,b2,b[2]];return 0}return et(a[1],a[2],f)}c(aa[12],l,k);return[0,f,c(aH[6],d,b[2])]}function
pY(a){var
b=0;function
c(c,a,b){return(b+a[1][4]|0)+a[1][3]|0}return e(aa[14],c,a,b)}var
bI=a(bF[1],[0,Z]);function
pZ(c,b){var
d=0;function
g(d,b,g){var
e=a(f[40],b);return x(n[1],c,p0,d,e)}return e(bI[13],g,b,d)}function
h$(k,a){function
d(m,e,l){var
b=m,a=l;for(;;){if(b){var
g=b[2],h=b[1],i=h[2],j=h[1];try{var
n=c(bI[27],j,k),o=c(f[6],n,i),p=d(g,c(f[1],e,o),a);return p}catch(c){c=v(c);if(c===M){var
b=g,a=[0,[0,j,i],a];continue}throw c}}return[0,e,a]}}return d(a,p1,0)}function
ia(g,e,d){function
b(a){var
b=c(f[4],a,e);return c(f[9],b,g)}var
h=d[2],i=d[1],j=a(f[25],g);if(0===j)return c(b1[6],d,e)?p2:a(l[2],p3);if(1===j){var
k=c(an,b,h);return[0,c(an,b,i),k]}var
m=c(an,b,i);return[0,c(an,b,h),m]}function
fL(g,f,b){function
d(j,i,h){var
b=h$(g,j),k=b[1],d=c(p[7][17],f,b[2]),m=d?d[1]:p6,n=ia(m,k,i[1][1]),e=c(b1[3],h,n);return e?e[1]:a(l[2],p5)}return e(aa[14],d,b,p4)}function
ib(d){var
e=d[1];if(e){var
g=d[2],b=e[1];if(g){var
h=g[1];if(c(f[29],b,p7))if(c(f[29],p8,h))return p9;var
k=a(f[22],h),l=a(f[24],b);return c(f[29],l,k)?a(f[24],b):b}return c(f[29],b,p_)?p$:a(f[24],b)}var
i=d[2];if(i){var
j=i[1],m=a(f[22],j);return c(f[29],qa,m)?qb:a(f[22],j)}return qc}function
ic(h,l,k,d,b){function
e(b,f){var
m=a(l,b);try{var
q=function(a){return a[1][1]!==h?1:0},d=c(g[33],q,m)[1],j=d[1],r=e(h_(j,d[2],d[3],d[4],b),[0,[0,j,b],f]);return r}catch(d){d=v(d);if(d===M){var
n=a(k,b);try{var
o=function(a){return a[1]!==h?1:0},i=c(g[33],o,n)[1],p=e(h8(i,b),[0,[0,i,b],f]);return p}catch(a){a=v(a);if(a===M)return[0,[0,b,f]];throw a}}throw d}}return e(d,b)}function
fM(d,c,b,a){try{var
e=ic(d,c,b,h6(a),0);return e}catch(a){a=v(a);if(a[1]===b2)return[1,a[2]];throw a}}function
id(w,v){var
d=v,c=0,i=0,j=0,h=0;for(;;){if(d){var
k=d[2],n=d[1],b=n[2],l=n[1];if(l){var
o=l[2],p=l[1],x=p[2];if(w===p[1]){var
m=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(b),q=b[1],r=q[2],s=q[1];if(1===a(f[25],x)){var
y=m(h,r),d=k,c=[0,[0,o,b],c],i=m(i,s),h=y;continue}var
z=m(h,s),d=k,c=[0,[0,o,b],c],i=m(i,r),h=z;continue}var
d=k,c=[0,[0,l,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
d=k,c=[0,[0,0,b],c],j=(b[4]+b[3]|0)+j|0;continue}var
t=a(g[1],i),A=0,B=function(b,a){return b+a|0},C=e(g[20],B,A,i),u=a(g[1],h),D=0,E=function(b,a){return b+a|0};return[0,c,j+u*C+t*e(g[20],E,D,h)-u*t]}}var
fN=[0,id,function(a){var
b=a[2],d=[0,0,fK(a)];function
f(b,a){var
d=a[1],c=id(b,a[2]);return[0,[0,[0,b,c[2]],d],c[1]]}var
h=e(aH[15],f,b,d)[1];function
i(b,a){return j1(b[2],a[2])}return c(g[48],i,h)}];function
fO(a){var
b=a[1];if(b){var
d=a[2];if(d)return c(f[26],b[1],d[1])}return 0}function
qd(a,h){var
b=a[1];if(b){var
d=a[2];if(d){var
e=d[1],g=c(f[26],b[1],e);return g?c(f[26],h,e):g}}return 0}function
fP(b,e){var
a=e;for(;;){if(a){var
c=a[2],d=a[1][1];if(d===b)return[0,1,c];if(d<b){var
a=c;continue}return[0,0,a]}return qe}}function
ie(d){var
a=d;for(;;){if(a){var
b=a[1],c=b[1],e=a[2],f=b[4],g=b[3],h=b[2];if(c)if(!c[2])return[0,[0,c[1][1],c,h,g,f]];var
a=e;continue}return 0}}function
ig(f,k){var
j=ie(f);if(j)return[0,j[1]];var
b=f;a:for(;;){if(b){var
c=b[1],i=c[1],a=i,o=b[2],p=c[4],q=c[3],r=c[2];for(;;){if(a){var
h=a[1][1],n=a[2],l=0,m=function(d){return function(a,b){var
c=b[2];return fP(d,b[1])[1]?fO(c[1])?a+1|0:a:a}}(h);if(2!==e(g[20],m,l,k)){var
a=n;continue}var
d=[0,h]}else
var
d=0;if(d)return[0,[0,d[1],i,r,q,p]];var
b=o;continue a}}return 0}}var
fQ=[0,fO,qd,fP,ie,ig,function(i){var
h=fK(i),j=0;function
k(b,d){var
a=d[2],e=a[1],g=e[1],j=d[1];if(g){var
h=e[2];if(h){var
i=g[1];return c(f[26],i,h[1])?[0,[0,j,i,a[2],a[4]+a[3]|0],b]:b}}return b}var
b=e(g[20],k,j,h),d=ig(b,h);if(d){var
a=d[1];return[0,[0,[0,a[1],a[2],a[3],a[4]],0],0]}var
l=0;function
m(s,f){var
p=f[1],e=p,n=h,k=s,t=f[4],u=f[3],v=f[2];a:for(;;){if(e){var
o=e[1][1],c=n,b=0,a=0,q=e[2],r=t-1|0;for(;;){if(c){var
g=c[2],l=c[1],d=l[2],i=d[3]+d[4]|0,m=fP(o,l[1]),j=m[2];if(0===m[1]){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}if(fO(d[1])){var
c=g,b=b+i|0,a=[0,[0,j,d],a];continue}var
c=g,b=(b+i|0)+r|0,a=[0,[0,j,d],a];continue}var
e=q,n=a,k=[0,[0,[0,o,p,v,u],b],k];continue a}}return k}}var
n=e(g[20],m,l,b);function
o(b,a){return Z(b[2],a[2])}return c(g[48],o,n)}];function
qf(h,d){var
i=0;function
j(d,b){var
e=a(p[7][18],b[1]);return c(l[5],d,e)}var
b=e(g[20],j,i,d),k=[0,[0,e(p[7][11],b,qh,h),0,qg],d],f=fM(b,fQ[6],fN[2],k);if(0===f[0]){var
m=f[1][1];try{var
q=[0,fL(bI[1],b,m[1])];return q}catch(b){b=v(b);if(a(bn[20],b)){var
o=a(d_[1],b);c(n[2],qi,o);return 0}throw b}}return 0}var
qj=[0,qf,function(i){var
d=fM(l[7],fQ[6],fN[2],i);if(0===d[0]){var
c=d[1][2],b=bI[1];for(;;){if(c){var
f=c[1],h=f[1],j=c[2],k=ib(fL(b,h,f[2][1])),c=j,b=e(bI[4],h,k,b);continue}var
m=0,n=function(c,b,a){return[0,[0,c,b],a]},o=e(bI[13],n,b,m);return[0,a(g[9],o)]}}return[1,d[1]]}];function
bo(b,a){return ev(b,a)[1]}function
ih(d,b,a){var
f=0;function
h(b,f){function
h(a,e){var
b=c(d,f,e);return b?[0,b[1],a]:a}return e(g[20],h,b,a)}return e(g[20],h,f,b)}function
ew(b,a){if(0===b)if(0===a)return 0;return 1}function
fR(s,r,q){var
j=q[2],k=q[1],l=r[2],m=r[1],n=j[3],e=j[2],g=j[1],o=l[3],h=l[2],i=l[1],t=c(p[7][17],s,i),u=c(p[7][17],s,g);if(t)if(u){var
b=u[1],d=t[1],v=a(f[25],b);if(-1===eW(a(f[25],d),v)){var
w=a(f[15],b),x=c(f[9],n,w),y=a(f[15],d),z=c(f[9],o,y),A=c(f[1],z,x),B=ew(h,e),C=[0,g,a(f[15],b)],D=[0,bo([0,i,a(f[15],d)],C),B,A],E=[0,k,a(f[15],b)];return[0,[0,bo([0,m,a(f[15],d)],E),D]]}if(0===h){var
F=c(f[9],n,qk),G=c(f[9],d,b),H=a(f[3],G),I=c(f[9],o,H),J=c(f[1],I,F),K=ew(h,e),L=c(f[9],d,b),M=[0,bo([0,i,a(f[3],L)],[0,g,ql]),K,J],N=c(f[9],d,b);return[0,[0,bo([0,m,a(f[3],N)],[0,k,qm]),M]]}if(0===e){var
O=c(f[9],o,qn),P=c(f[9],b,d),Q=a(f[3],P),R=c(f[9],n,Q),S=c(f[1],R,O),T=ew(h,e),U=c(f[9],b,d),V=[0,bo([0,g,a(f[3],U)],[0,i,qo]),T,S],W=c(f[9],b,d);return[0,[0,bo([0,k,a(f[3],W)],[0,m,qp]),V]]}return 0}return 0}function
ii(a){function
b(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=fJ(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b}return e(g[20],b,qq,a)}function
ij(t,e,s){var
j=e[2],k=e[1],m=e[4][1],u=m[2],v=m[1];function
n(e,b,a){if(b){var
f=b[1][3];if(a){var
d=a[1];return c(e,f,d)?[0,[0,k,j,d]]:b}return b}return a?[0,[0,k,j,a[1]]]:0}var
b=n(f[29],t,v),d=n(f[30],s,u);if(b)if(d){var
g=d[1],h=g[2],o=g[1],i=b[1],p=i[1],w=i[2];if(c(f[29],i[3],g[3]))return[0,[0,b,d]];var
q=h[1];if(q){var
r=fR(q[1][1],[0,p,w],[0,o,h]);return r?[1,r[1]]:a(l[2],qr)}return[1,[0,bo([0,p,qt],[0,o,qs]),h]]}return[0,[0,b,d]]}var
B=[0,ph,an,es,pi,pj,b1,aH,po,aa,h1,b2,pq,cK,fH,h2,pw,pA,pE,h3,pI,pJ,h4,h5,et,eu,pQ,fI,fJ,h6,fK,ev,h7,h8,h9,h_,pY,bI,pZ,h$,ia,fL,ib,ic,fM,fN,fQ,qj,[0,bo,ih,ew,fR,ii,ij,function(t,a){function
b(a){switch(a[0]){case
0:var
i=a[1];return[0,[0,[0,[0,i,qu],0],c(g[7],t,i)],0];case
1:var
u=a[3],v=a[1],w=b(a[2]),x=b(u);return ih(function(a,b){return fR(v,a,b)},w,x);default:var
y=a[2],z=b(a[1]),A=b(y),f=ii(c(l[25],z,A));if(0===f[0]){var
B=f[1],C=function(a,c){if(0===a[0]){var
b=a[1];return ij(b[1],c,b[2])}return a},h=e(g[20],C,qv,B);if(0===h[0]){var
j=h[1],d=j[2],k=j[1];if(k){var
m=k[1],n=m[2],o=m[1];if(d){var
p=d[1];return[0,[0,o,n],[0,[0,p[1],p[2]],0]]}var
r=n,q=o}else{if(!d)return 0;var
s=d[1],r=s[2],q=s[1]}return[0,[0,q,r],0]}return[0,h[1],0]}return[0,f[1],0]}}return b(a)}]];aU(698,B,"Micromega_plugin.Mfourier");var
qw=0,qx=0,qy=0,qz=r[13],qA=r[12][8],qC=0;function
qD(b){return[1,a(k[29][7],b)]}var
cL=[0,k[30][7],qD,qC,qB,qA,qz],qE=r[77],qF=r[80],qI=k[29][9],bp=[0,function(b){return[0,a(k[30][7],b),0]},qI,qH,qG,qF,qE];function
cM(e,b){function
d(b){switch(b[0]){case
0:var
g=a(e[2],b[1]);return a(p[6][4],g);case
1:var
h=a(k[29][2],b[1]);return a(p[6][2],h);case
2:var
i=b[1],j=d(b[2]),l=d(i);return c(p[6][7],l,j);case
3:var
m=b[1],n=d(b[2]),o=a(p[6][8],n),q=d(m);return c(p[6][7],q,o);case
4:var
r=b[2],s=d(b[1]),t=d(r);return c(p[6][6],s,t);case
5:var
u=d(b[1]);return a(p[6][8],u);default:var
v=b[2],w=d(b[1]),x=a(k[29][3],v),f=function(b){if(0===b){var
d=a(e[2],e[4]);return a(p[6][4],d)}var
g=f(b-1|0);return c(p[6][6],w,g)};return f(x)}}return d(b)}function
ik(b){function
c(f,c,b){var
d=a(k[30][2],f),e=1===c?[1,d]:[6,[1,d],a(k[30][3],c)];return V(b,qK)?e:[4,e,b]}return e(p[5][12],c,b,qJ)}function
il(o,h){function
p(b){var
d=a(f[24],b);return c(f[26],d,b)}if(c(g[27],p,h)){var
i=function(a){return ik(c(g[7],o,a))},e=qO,b=0,d=h;for(;;){if(d){var
j=d[2],l=d[1];if(c(f[26],l,qL)){var
b=b+1|0,d=j;continue}var
q=a(k[18],l),m=[0,a(k[30][7],q)],n=V(m,qM)?i(b):[4,m,i(b)],r=V(e,qN)?n:[2,n,e],e=r,b=b+1|0,d=j;continue}return e}}throw[0,am,qP]}function
im(e,d){var
b=d;for(;;){var
c=a(e,b);if(V(c,b))return c;var
b=c;continue}}function
io(b,e){var
d=x(r[74],b[3],b[4],b[5],b[6]);function
c(b){if(typeof
b!=="number")switch(b[0]){case
3:var
e=b[1],f=c(b[2]);return a(d,[3,c(e),f]);case
4:var
g=b[1],h=c(b[2]);return a(d,[4,c(g),h])}return a(d,b)}return c(e)}function
ex(b,a){return im(function(a){return io(b,a)},a)}function
ip(d){function
j(p,o){var
f=p,d=o;for(;;){if(f){var
g=f[2],a=f[1];if(d){var
c=d[1];if(typeof
c==="number")var
b=0;else
switch(c[0]){case
2:if(typeof
a==="number")var
b=0;else
if(2===a[0]){var
h=c[1],k=a[2],l=c[2];if(V(h,a[1]))var
e=[0,[2,h,[4,l,k]]],b=1;else
var
e=0,b=1}else
var
b=0;break;case
3:if(typeof
a==="number")var
b=0;else
if(3===a[0]){var
i=c[1],m=a[2],n=c[2];if(V(i,a[1]))var
e=[0,[3,i,[4,n,m]]],b=1;else
var
e=0,b=1}else
var
b=0;break;default:var
b=0}if(!b)var
e=0;if(e){var
f=g,d=[0,e[1]];continue}return[4,c,j(g,[0,a])]}var
f=g,d=[0,a];continue}return d?d[1]:0}}var
a=d,b=0,f=0;for(;;){if(typeof
a!=="number"&&4===a[0]){var
e=[0,a[1],b],a=a[2],b=e;continue}return j(c(g[48],G.caml_compare,[0,a,b]),f)}}function
iq(b,f){var
h=0;function
i(d,a){return[0,c(p[6][1],b,a),d]}var
d=e(g[20],i,h,f);if(V(b,p[5][1])){var
j=[1,q[1]],k=a(g[9],d);return[0,a(p[7][5],[0,[1,q[2]],k]),0,j]}var
l=[1,q[1]],m=a(g[9],d);return[0,a(p[7][5],[0,[1,q[1]],m]),0,l]}function
ir(a){function
c(f,d){var
b=f,a=d;for(;;){if(a){if(0===a[1][2]){var
b=b+1|0,a=a[2];continue}var
g=c(b+1|0,a[2]),h=1,i=p[7][3],j=function(a){return qR};return[0,[0,e(p[7][10],b+1|0,j,i),h,qQ],g]}return 0}}return c(0,a)}function
qS(a){switch(a){case
0:return qT;case
1:return qU;case
2:return qV;default:return qW}}var
ey=a(d9[1],[0,p[5][10]]);function
is(b){function
f(a){return a[1]}var
d=c(g[17],f,b),h=a(ey[5],p[5][1]);function
i(b,a){function
d(b,d,a){return c(ey[4],b,a)}return e(p[6][9],d,a,b)}var
j=e(g[20],i,h,d),k=0;function
m(b,a){return[0,iq(b,d),a]}var
n=e(ey[15],m,j,k),o=[1,q[2]],r=1;function
s(a){return 2===a[2]?[1,q[2]]:[1,q[1]]}var
t=c(g[17],s,b),u=[0,a(p[7][5],[0,[1,q[2]],t]),r,o],v=[0,u,ir(b)],w=c(l[25],v,n),x=[1,q[1]];return[0,[0,a(p[7][5],[0,[1,q[2]],0]),1,x],w]}var
qX=k[30][7];function
fS(d,b){var
e=b[1],g=d[1],f=b[2];if(e){var
i=e[2],h=function(d,b){if(d){var
e=d[1],m=d[2];if(b){var
i=b[1],f=h(m,b[2]),j=c(q[23],e,q[1]);if(-1===j){var
n=[0,a(k[30][1],i)];return[4,[2,[0,a(g,e)],n],f]}if(0===j)return f;var
o=[0,a(k[30][1],i)];return[4,[3,[5,a(g,e)],o],f]}return a(l[2],qY)}return 0};return ip(ex(d,h(i,f)))}return a(l[2],qZ)}var
q1=[aW,q0,aV(0)],fT=[aW,q2,aV(0)],aI=a(bF[1],[0,p[5][10]]);function
it(b){var
d=[0,0];function
o(b,a){return Z(b[1],a[1])}var
h=[0,aI[1],0];function
i(i,h){var
j=i[2],k=i[1],q=h[2],m=[0,h[1],0];function
n(g,j,i){var
h=i[2],b=i[1];if(V(g,p[5][1]))return[0,b,h];try{var
q=c(aI[27],g,b),l=q,k=b}catch(a){a=v(a);if(a!==M)throw a;var
m=e(aI[4],g,d[1],b),n=d[1];d[1]++;var
l=n,k=m}var
o=0===a(f[25],j)?h:[0,[0,l,j],h];return[0,k,o]}var
b=e(p[6][9],n,k,m),r=b[2],s=b[1],t=c(p[6][1],p[5][1],k),u=a(f[3],t);if(0===j)var
l=0;else{if(!(3<=j))throw fT;var
l=1}return[0,s,[0,[0,c(g[48],o,r),l,u],q]]}return e(g[21],i,b,h)[2]}function
iu(e){var
f=is(e);try{var
b=a(B[47][2],f);if(0===b[0])var
h=a(p[7][8],b[1]),d=[0,a(k[22],h)];else
var
d=0;return d}catch(b){b=v(b);if(a(bn[20],b)){if(B[4]){var
g=a(d_[1],b);c(n[2],q3,g);a(l[51],l[27])}return 0}throw b}}function
iv(b){try{var
d=it(b),f=a(B[47][2],d);if(0===f[0])var
h=0;else{var
i=f[1];if(B[4])e(n[2],q4,B[13],i);var
l=c(B[48][7],d,i),m=a(g[5],l)[1],o=function(a){return[0,a[1]+1|0,a[2]]},j=c(g[17],o,m);if(B[4])e(n[2],q5,p[7][4],j);var
q=a(p[7][8],j),h=[0,a(k[22],q)]}return h}catch(a){a=v(a);if(a===fT)return iu(b);throw a}}function
iw(d){var
b=a(g[46],d),e=b[2],c=iv(b[1]);return c?[0,[0,c[1],e]]:0}function
q6(e,b){var
d=a(g[1],b)-1|0,f=c(k[14],0,d),h=c(g[47],b,f);function
i(a){return 1===a[1][2]?1:0}var
j=c(g[37],i,h)[2];function
m(b){var
c=b[1],d=c[2],f=b[2],g=c[1];return 1===d?a(l[2],q7):[0,[0,cM(e,g),d],f]}return iw(c(g[17],m,j))}function
ix(c,b){try{var
e=q6(c,b);return e}catch(b){b=v(b);if(a(bn[20],b)){var
d=a(d_[1],b);a(l[30],d);return 0}throw b}}function
cN(e,b){var
d=a(g[1],e),f=c(l[5],b,eW(d,b));return c(l[5],d,f)}function
q8(d,b,a){var
e=cN(a,d);B[10][1]=e;var
c=ix(b,a);return c?[0,fS(b,c[1])]:0}function
q9(n,b){a(p[22][1][6],0);var
o=cN(b,n);B[10][1]=o;function
q(c,b){return[0,c,[0,a(k[30][1],b)]]}var
f=c(k[23],q,b);function
s(b,a){var
d=a[1],e=b[1],f=d[1],g=e[1],h=[3,b[2],a[2]];return[0,[0,[4,g,f],c(r[53],e[2],d[2])],h]}var
t=c(k[9],s,f),u=0;function
v(b,a){var
c=a[1],d=c[2],e=a[2],f=c[1];return d?[0,[0,[0,f,d[1]],e],b]:b}var
w=e(g[20],v,u,t),h=c(l[25],f,w);function
x(a){return cM(bp,a[1][1])}var
y=c(g[17],x,h),z=aI[1];function
A(c,b){function
d(c,f,b){var
d=a(p[5][13],c);return d?e(aI[4],d[1],c,b):b}return e(p[6][9],d,b,c)}var
C=e(g[20],A,z,y);function
i(b){var
c=[0,bp[4]];function
d(d,c,b){var
e=a(k[30][3],c);return[4,[6,[1,a(k[30][2],d)],e],b]}return e(p[5][12],d,b,c)}var
D=d0(r[63],bp[3],bp[4],r[79],r[80],r[82],r[81],r[77]),E=0;function
F(d,c,b){var
e=[1,a(D,i(d))];return[0,[0,[0,i(c),3],e],b]}var
G=e(aI[13],F,C,E),j=c(l[25],h,G);function
H(a){return a[1]}var
m=ix(bp,c(g[17],H,j));if(m){var
I=fS(bp,m[1]),d=function(b){if(typeof
b==="number")return 0;else
switch(b[0]){case
0:var
e=a(k[29][1],b[1]);return c(g[7],j,e)[2];case
1:return[1,b[1]];case
2:var
f=b[1];return[2,f,d(b[2])];case
3:var
h=b[1],i=d(b[2]);return[3,d(h),i];case
4:var
l=b[1],m=d(b[2]);return[4,d(l),m];default:return[5,b[1]]}};return[0,d(I)]}return 0}function
q_(b){function
h(a){return a[1]}var
i=c(g[17],h,b),j=a(p[6][4],q$);function
k(b,a){return c(p[6][7],a,b)}var
l=e(g[20],k,j,i),m=0;function
n(b,c,a){return V(b,p[5][1])?a:[0,b,a]}var
d=e(p[6][9],n,l,m);function
o(b){var
e=b[1],h=b[2],i=c(p[6][1],p[5][1],e),j=a(f[3],i);function
k(a){return c(p[6][1],a,e)}var
l=c(g[17],k,d);return[0,a(p[7][5],l),h,j]}return[0,c(g[17],o,b),d]}function
ra(b,a){return[2,b,a]}function
rb(b,a){return[4,b,a]}function
rc(a){return[0,a]}function
rd(a){return[5,a]}function
re(e,d,b){var
a=b;for(;;){if(a){var
f=a[2];if(c(e,d,a[1]))return 1;var
a=f;continue}return 0}}function
iy(d,b,g){var
a=g;for(;;){if(a){var
e=a[2],f=a[1];if(c(d,b,f[1])){var
a=e;continue}return[0,f,iy(d,b,e)]}return 0}}function
iz(b,a){return 0===c(p[7][15],b,a)?1:0}function
rf(c,a){var
b=0;function
d(b,a){return iz(a,c)?b:[0,a,b]}return e(g[20],d,b,a)}function
fU(b,a){var
c=a[2],d=a[1];if(0===c)return[0,cM(b,d),0];if(3<=c)return[0,cM(b,d),1];throw[0,am,rg]}function
iA(a){return 0===a?0:3}function
rh(d){var
b=a(g[46],d),e=b[1],f=a(k[22],b[2]);function
h(a){return[1,a]}var
i=c(g[17],h,f);return c(g[47],e,i)}function
iB(b){var
f=b[3],h=b[2],e=a(g[46],b[1]),i=e[1],d=a(k[22],[0,f,e[2]]);if(d){var
j=d[2],l=[1,d[1]],m=function(a){return[1,a]},n=c(g[17],m,j);return[0,c(g[47],i,n),h,l]}throw[0,am,ri]}function
rj(e,d){var
b=iB(d),c=b[3],f=b[2],g=b[1];try{var
h=il(e,a(p[7][8],g)),i=a(k[17],c),j=a(k[30][7],i),m=a(k[18],c),n=a(k[30][7],m),o=[0,[2,[4,[0,j],h],[5,[0,n]]],iA(f)];return o}catch(b){b=v(b);if(b[1]===e_)return a(l[2],rk);throw b}}function
bJ(b){if(typeof
b==="number")return[0,q[2],0];else
switch(b[0]){case
0:var
e=b[1],z=[0,[1,a(k[18],e)]];return[0,a(k[17],e),z];case
1:return[0,q[2],[1,b[1]]];case
2:return a(l[2],rl);case
3:var
f=bJ(b[1]);return[0,f[1],[3,f[2]]];case
4:var
g=b[1],A=g[2],h=bJ(g[1]),i=h[2],j=h[1],m=bJ(A),n=m[2],o=m[1],d=c(q[17],j,o),p=c(q[15],j,d),r=c(q[15],o,d),B=c(q[10],p,r),s=c(q[10],d,B);return 0===c(q[23],s,q[2])?[0,q[2],[4,[0,i,n]]]:[0,s,[4,[0,[6,[0,[0,[1,r]],i]],[6,[0,[0,[1,p]],n]]]]];case
5:return a(l[2],rm);case
6:var
t=b[1],C=t[2],u=bJ(t[1]),D=u[2],E=u[1],v=bJ(C),F=[6,[0,D,v[2]]];return[0,c(q[10],E,v[1]),F];case
7:return a(l[2],rn);default:var
w=b[1],x=w[2],y=bJ(w[1]),G=[8,[0,y[2],x]];return[0,c(q[19],y[1],x),G]}}function
fV(b){var
a=bJ(b);return[0,a[1],a[2]]}function
ro(h,g,f){var
d=0,c=0,b=f;for(;;){if(b){var
e=b[2];if(a(h,b[1])){if(d===g)return c;var
d=d+1|0,c=c+1|0,b=e;continue}var
c=c+1|0,b=e;continue}return a(l[2],rp)}}function
bK(b){switch(b[0]){case
0:return[0,q[2],[0,b[1]]];case
1:return[0,q[2],[1,b[1]]];case
2:return[0,q[2],[2,b[1]]];case
3:var
e=b[1],v=[3,[1,a(k[18],e)]];return[0,a(k[17],e),v];case
4:var
f=b[1],w=[4,[1,a(k[18],f)]];return[0,a(k[17],f),w];case
5:var
g=b[1],x=[5,[1,a(k[18],g)]];return[0,a(k[17],g),x];case
6:var
h=fV(b[1]),i=h[1],y=[6,h[2]];return[0,c(q[10],i,i),y];case
7:return[0,q[2],[7,b[1]]];case
8:var
z=b[2],j=fV(b[1]),A=j[2],B=j[1],l=bK(z),C=[8,A,l[2]];return[0,c(q[10],B,l[1]),C];case
9:var
D=b[2],m=bK(b[1]),n=m[1],E=m[2],o=bK(D),p=o[1],F=o[2],d=c(q[17],n,p),r=c(q[15],n,d),s=c(q[15],p,d),G=c(q[10],r,s);return[0,c(q[10],d,G),[9,[10,[4,[1,s]],E],[10,[4,[1,r]],F]]];default:var
H=b[2],t=bK(b[1]),I=t[2],J=t[1],u=bK(H),K=[10,I,u[2]];return[0,c(q[10],J,u[1]),K]}}function
aJ(b){if(typeof
b==="number")return[0,a(k[30][8],rq)];else
switch(b[0]){case
0:return[0,a(k[30][8],b[1])];case
1:var
c=b[1],i=j3(e(iC[4],c,1,j2(c)-1|0));return[1,a(k[30][4],i)];case
3:return[5,aJ(b[1])];case
4:var
d=b[1],j=d[1],m=aJ(d[2]);return[2,aJ(j),m];case
5:var
f=b[1],n=f[1],o=aJ(f[2]);return[3,aJ(n),o];case
6:var
g=b[1],p=g[1],q=aJ(g[2]);return[4,aJ(p),q];case
8:var
h=b[1],r=h[1],s=a(k[30][3],h[2]);return[6,aJ(r),s];default:return a(l[2],rr)}}function
fW(b){var
c=aJ(b),d=r[77],e=r[81],f=r[82],g=r[80],h=r[79],i=a(k[30][8],rs),j=a(k[30][8],rt);return bO(r[39],j,i,h,g,f,e,d,c)}function
ez(b){if(b){var
c=b[2],d=b[1];if(c){var
e=ez(c);return[3,[0,a(k[30][1],d)],e]}return[0,a(k[30][1],d)]}return 0}function
ru(b){function
d(b){switch(b[0]){case
0:return[0,a(k[30][1],b[1])];case
1:return[0,a(k[30][1],b[1])];case
2:return[0,a(k[30][1],b[1])];case
6:return[1,fW(b[1])];case
7:return ez(b[1]);case
8:var
g=b[1],h=d(b[2]);return[2,fW(g),h];case
9:var
i=b[1],j=d(b[2]);return[4,d(i),j];case
10:var
l=b[1],m=d(b[2]);return[3,d(l),m];default:var
e=b[1];return 0===c(f[37],e,rv)?0:[5,a(k[30][8],e)]}}return ex(bp,d(b))}function
aK(b){if(typeof
b==="number")return rw;else
switch(b[0]){case
0:var
j=a(f[52],b[1]);return[0,a(k[30][7],j)];case
1:var
c=b[1],m=j3(e(iC[4],c,1,j2(c)-1|0));return[1,a(k[30][4],m)];case
3:return[5,aK(b[1])];case
4:var
d=b[1],n=d[1],o=aK(d[2]);return[2,aK(n),o];case
5:var
g=b[1],p=g[1],q=aK(g[2]);return[3,aK(p),q];case
6:var
h=b[1],r=h[1],s=aK(h[2]);return[4,aK(r),s];case
8:var
i=b[1],t=i[1],u=a(k[30][3],i[2]);return[6,aK(t),u];default:return a(l[2],rx)}}function
fX(b){var
c=aK(b),d=r[13],e=r[12][6],f=r[12][7],g=r[12][8],h=r[12][5],i=a(k[30][5],1),j=a(k[30][5],0);return bO(r[39],j,i,h,g,f,e,d,c)}function
ry(b){var
e=bK(b)[2];function
d(l){var
b=l;for(;;)switch(b[0]){case
0:return[0,a(k[30][1],b[1])];case
1:return[0,a(k[30][1],b[1])];case
2:return[0,a(k[30][1],b[1])];case
6:return[1,fX(b[1])];case
7:return ez(b[1]);case
8:var
i=b[2],e=b[1];if(typeof
e==="number")var
g=0;else
if(0===e[0])var
j=c(f[26],e[1],rA),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
b=i;continue}var
n=d(i);return[2,fX(e),n];case
9:var
o=b[1],p=d(b[2]);return[4,d(o),p];case
10:var
q=b[1],r=d(b[2]);return[3,d(q),r];default:var
h=b[1];if(0===c(f[37],h,rz))return 0;var
m=a(f[52],h);return[5,a(k[30][7],m)]}}return ex(cL,d(e))}var
iD=[0,function(e,d){var
c=0,b=d;for(;;){if(b){var
f=b[2];if(V(e,b[1]))return c;var
c=c+1|0,b=f;continue}return a(l[2],rB)}}];function
fY(b){var
c=b[1],d=a(k[18],b[2]),f=[0,a(k[30][7],d)];function
h(f,b){var
g=b[2],h=a(p[22][1][8],b[1]);function
c(d,c,b){var
e=a(k[30][3],c);return[4,[6,[1,a(k[30][2],d)],e],b]}var
d=e(p[5][12],c,h,rC),i=a(k[18],g);return[2,[4,[0,a(k[30][7],i)],d],f]}return e(g[20],h,f,c)}function
aL(d,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
2:return[5,a(k[30][7],b[1])];case
3:var
f=fY(b[1]);return[1,a(r[92],f)];case
4:var
g=b[2],h=fY(b[1]),i=a(r[92],h);return[2,i,aL(d,g)];case
6:var
j=b[1],m=aL(d,b[2]);return[3,aL(d,j),m];case
7:var
n=b[1],o=aL(d,b[2]);return[4,aL(d,n),o];case
0:case
1:var
e=c(iD[1],b[1],d);return[0,a(k[30][1],e)];default:return a(l[2],rD)}}function
cO(b,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
e=a[3],d=a[2],f=a[1];if(typeof
d!=="number"&&8===d[0]){var
i=d[1],j=cO([0,f,b],e);return[1,aL(b,i),j]}var
h=cO([0,f,b],e);return[0,aL(b,d),h]}var
k=a[5],l=a[4],m=a[2],n=[0,a[1],b],o=function(a){return cO(n,a)},p=c(g[17],o,k),q=aL(b,l);return[2,aL(b,m),q,p]}}function
iE(e,b){var
f=1+a(p[15],b)|0,d=c(p[18],f,b)[2];if(B[4])x(n[1],l[27],rE,p[13],d);return cO(e,d)}function
eA(b){var
d=a(B[47][2],b);if(0===d[0])return 0;var
f=d[1];if(B[4])e(n[2],rF,B[13],f);var
i=c(B[48][7],b,f),h=a(g[5],i)[1];if(B[4])e(n[2],rG,p[7][4],h);var
j=a(p[7][8],h);return[0,a(k[22],j)]}function
rH(d,b){var
e=a(f[40],b);return c(l[54],d,e)}function
rI(d,b){var
e=a(q[33],b);return c(l[54],d,e)}function
eB(g,f){var
e=0,d=g,b=f;for(;;){if(b){if(d){var
h=b[2],i=d[2],j=c(p[20],b[1],d[1]),e=c(p[19],j,e),d=i,b=h;continue}return a(l[2],rJ)}return e}}function
iF(d){var
b=a(g[46],d),e=b[2],c=eA(b[1]);return c?[0,eB(e,c[1])]:0}var
iG=B[4]?function(b){a(n[2],rK);a(l[51],l[27]);var
c=iF(b);a(n[2],rL);a(l[51],l[27]);return c}:iF;function
fZ(m){var
d=m[2],h=m[1],i=h[3],j=h[2],l=h[1];if(l){var
o=function(a){return a[2]},p=c(g[17],o,l),n=a(k[21],p),b=[1,n];if(c(f[32],b,rM))return[2,h,d];var
q=c(f[12],i,b);if(0===a(f[25],q)){if(1<=a(f[25],b)){var
r=c(f[9],i,b),s=function(a){var
d=a[1];return[0,d,c(f[9],a[2],b)]};return[2,[0,c(g[17],s,l),j,r],[5,n,d]]}throw[0,am,rN]}if(0===j)return[0,[8,d]];var
t=c(f[9],i,b),u=a(f[24],t),v=function(a){var
d=a[1];return[0,d,c(f[9],a[2],b)]};return[1,[0,c(g[17],v,l),j,u],[8,d]]}return e(B[26],j,rO,i)?0:[0,d]}function
iH(l,j,i){var
e=i[1],g=j[1],m=e[2],n=e[1],o=g[2],q=g[1],t=i[2],u=j[2],v=e[3],w=g[3];function
h(d,b){var
e=a(k[18],b),g=c(p[20],e,t),h=a(k[18],d),i=[7,c(p[20],h,u),g],j=c(f[6],v,b),l=c(f[6],w,d),r=c(f[1],l,j),s=c(B[48][3],o,m),x=c(p[7][13],b,n),y=c(p[7][13],d,q);return[0,[0,c(p[7][14],y,x),s,r],i]}var
r=c(p[7][17],l,q),s=c(p[7][17],l,n);if(r)if(s){var
b=s[1],d=r[1],x=a(f[25],b);if(-1===eW(a(f[25],d),x)){var
y=a(f[15],b);return[0,h(y,a(f[15],d))]}if(0===o){var
z=[0,a(f[25],d)],A=c(f[6],b,z),C=a(f[3],A);return[0,h(C,a(f[15],d))]}if(0===m){var
D=a(f[15],b),E=[0,a(f[25],b)],F=c(f[6],d,E);return[0,h(D,a(f[3],F))]}return 0}return 0}var
cP=[aW,rP,aV(0)];function
iI(a){var
b=0;function
c(b,c){var
a=fZ([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,cP,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return e(g[20],c,b,a)}function
cQ(g,b){if(0===a(q[22],b))return[0,q[2],q[1]];var
d=c(q[14],g,b),h=d[1],e=cQ(b,d[2]),f=e[2],i=e[1],j=c(q[10],h,f);return[0,f,c(q[8],i,j)]}function
rQ(i,h){var
b=a(q[36],i),d=a(q[36],h),e=cQ(b,d),f=e[2],g=e[1],j=c(q[10],f,d),k=c(q[10],g,b),m=c(q[5],k,j),o=a(q[33],m),p=a(q[33],d),r=a(q[33],f),s=a(q[33],b),t=a(q[33],g);return d0(n[1],l[27],rR,t,s,r,p,o)}var
rT=[aW,rS,aV(0)];function
rU(c){function
b(a){return 0===a[1][2]?1:0}return a(g[37],b)}function
iJ(r,p){var
f=p[1],g=r[1];if(0===g[2])if(0===f[2]){var
d=g[1],b=f[1];for(;;){if(d)if(b){var
h=b[2],i=b[1],j=i[2],l=i[1],m=d[2],n=d[1],o=n[2],e=n[1];if(V(e,l)){var
s=q[2],t=a(k[18],j),u=a(k[18],o),v=c(q[17],u,t);if(0===c(q[23],v,s))return[0,[0,e,o,j]];var
d=m,b=h;continue}if(gB(e,l)){var
d=m;continue}var
b=h;continue}return 0}}return 0}function
iK(m,l){var
d=0,b=l;for(;;){if(b){var
f=b[2],e=b[1],n=a(m,e),h=c(k[15],n,f),i=h[1];if(i){var
j=i[1],o=j[2],p=j[1];return[0,[0,[0,p,e,o]],c(g[12],d,h[2])]}var
d=[0,e,d],b=f;continue}return[0,0,d]}}function
iL(a){return iK(iJ,a)}function
eC(f,b){var
c=0;function
d(c,d){var
e=a(f,d);if(e){var
b=fZ(e[1]);if(typeof
b==="number")return c;else
switch(b[0]){case
0:throw[0,cP,b[1]];case
1:return[0,[0,b[1],b[2]],c];default:return[0,[0,b[1],b[2]],c]}}return[0,d,c]}return e(g[20],d,c,b)}function
eD(c,b,a){return eC(function(a){return iH(c,b,a)},a)}function
iM(r){var
i=iL(r),j=i[1],s=i[2];if(j){var
b=j[1],l=b[3],m=l[1],n=b[2],o=n[2],d=n[1],e=b[1],t=l[2],u=e[2],v=e[1],w=a(k[18],e[3]),q=cQ(a(k[18],u),w),g=[1,q[1]],h=[1,q[2]],x=c(f[6],h,m[3]),y=c(f[6],g,d[3]),z=c(f[1],y,x),A=c(p[7][13],h,m[1]),B=c(p[7][13],g,d[1]),C=[0,c(p[7][14],B,A),0,z],D=a(k[18],h),E=c(p[20],D,t),F=a(k[18],g),G=c(p[20],F,o);return[0,eD(v,[0,C,c(p[19],G,E)],[0,[0,d,o],s])]}return 0}function
iN(e){function
h(b){var
a=b[1];if(0===a[2])try{var
d=a[1],e=function(d){var
a=d[2],b=c(f[26],a,rV);return b?b:c(f[26],a,rW)},h=[0,c(g[33],e,d)[1]];return h}catch(a){a=v(a);if(a===M)return 0;throw a}return 0}var
a=c(k[15],h,e),b=a[1],i=a[2];if(b){var
d=b[1];return[0,eD(d[1],d[2],i)]}return 0}function
iO(h){function
i(e){var
b=e[1];if(0===b[2])try{var
h=b[1],i=function(b){var
d=b[2],g=b[1],h=c(f[26],d,rX),e=h||c(f[26],d,rY);if(e){var
i=a(p[22][1][8],g);return a(p[5][4],i)}return e},d=c(g[33],i,h)[1],j=a(p[22][1][8],d),k=b[1],l=function(g){var
b=g[1],e=b===d?1:0;if(e)var
f=e;else
var
h=a(p[22][1][8],b),f=0===c(p[5][9],h,j)[2]?1:0;return f},m=c(g[27],l,k)?[0,d]:0;return m}catch(a){a=v(a);if(a===M)return 0;throw a}return 0}var
b=c(k[15],i,h),d=b[1],j=b[2];if(d){var
e=d[1];return[0,eC(c(p[22][9],e[1],e[2]),j)]}return 0}function
iP(r){function
s(i){var
b=i;for(;;){if(b){var
d=b[2],e=b[1],j=e[1],f=a(k[18],e[2]);try{var
l=function(g){return function(b){var
d=a(k[18],b[2]),e=q[2],f=c(q[17],g,d);return c(q[24],f,e)}}(f),h=c(g[33],l,d),m=h[1],n=[0,[0,[0,j,f],[0,m,a(k[18],h[2])]]];return n}catch(a){a=v(a);if(a===M){var
b=d;continue}throw a}}return 0}}function
t(b){var
a=b[1];return 0===a[2]?s(a[1]):0}var
b=c(k[15],t,r),d=b[1],u=b[2];if(d){var
e=d[1],h=e[2],i=h[1],j=e[1],l=j[2],m=j[1],w=h[2],x=l[1],y=m[1],n=cQ(m[2],l[2]),z=[1,n[2]],A=[1,n[1]],o=function(d,b){var
a=c(p[7][17],d,b);return a?a[1]:rZ};return[0,eC(function(g){var
b=g[1],d=b[1],h=g[2],j=b[3],k=b[2],l=o(y,d),m=o(x,d),n=c(f[6],m,z),q=c(f[6],l,A),r=c(f[1],q,n),e=a(f[3],r),s=c(f[6],e,i[3]),t=c(f[1],s,j),u=c(p[7][13],e,i[1]);return[0,[0,[0,c(p[7][14],u,d),k,t],[7,[4,[0,0,e],w],h]]]},u)]}return 0}function
r0(i){function
j(c){var
b=c[1];if(0===b[2])try{var
d=[0,a(g[5],b[1])[1]];return d}catch(a){a=v(a);if(a===M)return 0;throw a}return 0}var
d=c(k[15],j,i),e=d[1],l=d[2];if(e){var
h=e[1],b=h[2],m=h[1];if(B[4]){var
o=a(f[40],b[1][3]);x(n[2],r1,p[7][4],b[1][1],o)}return[0,eD(m,b,l)]}return 0}function
f0(e,d){var
b=d;for(;;){var
c=a(e,b);if(c){var
b=c[1];continue}return b}}function
f1(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1],d);if(c)return[0,c[1]];var
b=f;continue}return 0}}function
f2(a){var
b=[0,iN,[0,iM,[0,iP,0]]];return f0(function(a){return f1(b,a)},a)}function
iQ(a){var
b=[0,iO,0];return f0(function(a){return f1(b,a)},a)}function
iR(d){function
C(a){return 0===a[2]?1:0}var
r=c(g[37],C,d),s=r[2],t=r[1];if(t)var
D=0,E=function(b,a){function
d(b){return c(p[7][1],a[1],b[1])}return c(g[28],d,t)?b:[0,a[1],b]},u=e(g[20],E,D,s);else
var
F=function(a){return a[1]},u=c(g[19],F,s);var
G=[0,p[7][3],r3];function
H(b,g){var
h=a(B[6][4],b[2]),m=h?c(f[29],h[1],r2):0;if(m)return b;var
j=c(B[47][1],g,d);if(j){var
k=j[1];if(B[4])e(n[2],r4,p[7][4],g);var
i=b[2],l=b[1];return c(B[6][5],k,i)?[0,g,k]:[0,l,i]}return b}var
v=e(g[20],H,G,u),w=v[2],x=w[1],I=v[1];if(x){var
y=w[2];if(y)var
b=[0,[0,x[1],I,y[1]]],o=1;else
var
o=0}else
var
o=0;if(!o)var
b=0;if(b){var
h=b[1],i=h[3],j=h[2],m=h[1],J=a(k[17],m),K=q[2],L=a(k[18],m),M=c(q[8],L,K),N=a(k[17],i),O=a(k[18],i),P=[1,c(q[5],q[2],O)],z=eA([0,[0,c(p[7][13],[1,N],j),1,P],d]),Q=a(f[3],[1,M]),R=a(f[3],[1,J]),A=eA([0,[0,c(p[7][13],R,j),1,Q],d]);if(z)if(A){var
S=A[1],T=a(g[6],z[1]);return[0,[0,a(g[6],S),[0,m,j,i],T]]}return a(l[2],r5)}return 0}function
eE(b){function
d(b){var
d=b[1][1];function
e(b){return 0!==a(f[25],b[2])?1:0}return c(g[27],e,d)}return c(g[27],d,b)}function
f3(o,m,b){function
q(i,b){if(B[4]){a(n[2],r6);a(l[51],l[27])}if(eE(b)){var
j=a(g[46],b),k=j[2],m=iR(j[1]);if(m){var
c=m[1],d=c[2],o=d[3],e=d[2],q=d[1],s=c[3],t=c[1];if(B[4]){var
u=a(f[40],o),v=a(f[40],q);aT(n[2],r7,p[7][4],e,v,u)}var
w=a(f[22],o),r=h(i,e,a(f[24],q),w,b);if(r){var
x=r[1],y=eB(k,s);return[0,[1,i,eB(k,t),e,y,x]]}return 0}return 0}throw[0,am,r8]}function
h(b,g,a,e,d){if(c(f[28],a,e))return r9;var
j=i(b+1|0,[0,[0,[0,g,0,a],[1,b]],d]);if(j){var
l=j[1],k=h(b,g,c(f[1],a,r_),e,d);return k?[0,[0,l,k[1]]]:0}return 0}function
i(d,b){if(eE(b)){if(B[4]){var
h=function(b,a){return c(p[9],b,a[1])},i=a(k[2],h);e(n[2],r$,i,b)}try{var
f=a(m,b);if(B[4]){var
j=function(b,a){return c(p[9],b,a[1])},l=a(k[2],j);e(n[2],sa,l,f)}var
g=iG(f),r=g?[0,[0,d,g[1],0]]:o?q(d,f):0;return r}catch(a){a=v(a);if(a[1]===cP)return[0,[0,d,a[2],0]];throw a}}throw[0,am,sb]}var
j=a(g[1],b);try{var
t=i(j,iI(b)),d=t}catch(a){a=v(a);if(a[1]!==cP)throw a;var
d=[0,[0,j,a[2],0]]}if(d){var
r=d[1],s=function(b,a){return a};return[0,iE(c(k[23],s,b),r)]}return 0}function
f4(b){var
d=b[2],c=a(p[22][5],b[1]),e=c[1];return[0,e,d,a(f[3],c[2])]}function
sc(e,d,b){a(p[22][1][6],0);var
f=cN(b,d);B[10][1]=f;function
h(a){return fU(cL,a)}var
i=c(g[17],h,b),j=c(g[17],f4,i);function
l(b,a){return[0,b,[0,a]]}return f3(e,f2,c(k[23],l,j))}var
aB=[0,qw,qx,qy,cL,bp,cL,cM,ik,il,im,io,ex,ip,iq,ir,qS,ey,is,qX,fS,q1,fT,aI,it,iu,iv,iw,cN,q8,q9,q_,ra,rb,rc,rd,re,iy,iz,rf,fU,iA,rh,iB,rj,fV,ro,bK,aJ,fW,ez,ru,aK,fX,ry,iD,fY,aL,cO,iE,eA,rH,rI,eB,iG,fZ,iH,cP,iI,cQ,rQ,rT,rU,iJ,iK,iL,eC,eD,iM,iN,iO,iP,r0,f0,f1,f2,iQ,iR,eE,f3,f4,sc,function(n,m,f){a(p[22][1][6],0);var
o=cN(f,m);B[10][1]=o;function
q(a){return fU(cL,a)}var
r=c(g[17],q,f);function
s(b,a){return[0,b,[0,a]]}var
b=c(k[23],s,r);function
t(b){return a(p[6][13],b[1][1])}var
h=c(g[27],t,b),u=aI[1];function
v(c,b){var
d=b[1][1];function
f(c,f,b){var
d=a(p[5][13],c);return d?e(aI[4],d[1],c,b):b}return e(p[6][9],f,d,c)}var
w=e(g[20],v,u,b);function
x(d,c,b){var
f=a(p[6][4],sd),g=e(p[6][3],d,se,f),h=a(p[22][5],g),i=a(p[6][4],sf);return[0,[0,[0,e(p[6][3],c,sg,i),1],[3,h]],b]}var
d=e(aI[13],x,w,b);if(h)var
i=d;else
var
A=function(b,a){var
d=a[1],e=b[1],f=d[1],g=e[1],h=[6,b[2],a[2]],i=c(p[10],e[2],d[2]);return[0,[0,c(p[6][6],g,f),i],h]},C=c(k[8],A,d),i=c(l[25],d,C);function
y(a){var
b=a[2];return[0,f4(a[1]),b]}var
j=c(g[17],y,i);if(eE(j)){var
z=h?f2:iQ;return f3(n,z,j)}throw[0,am,sh]}];aU(700,aB,"Micromega_plugin.Certificate");var
cR=[0,function(p){var
b=a(bj[19],p),k=[aW,si,aV(0)],f=[aW,sj,aV(0)];function
q(d,c){var
f=a(b[1],d),g=e(D[23],c,sk,eY);return[0,a(D[31],g),1,f]}function
r(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=v(b);try{a(c,0)}catch(a){throw b}throw b}}function
s(b){try{var
c=[0,a(d$[3],b)];return c}catch(b){b=v(b);if(b===sl)return 0;if(a(bn[20],b))throw k;throw b}}function
t(c,a){var
d=e(D[34],a,0,1);try{e(D[34],a,0,0);var
f=0===c?4:1;e(D[83],a,f,1);var
g=1,b=g}catch(a){a=v(a);if(a[1]!==D[1])throw a;var
b=0}e(D[34],a,d,0);return b}function
u(a){var
c=e(D[34],a,0,1);try{e(D[34],a,0,0);var
b=e(D[83],a,0,1);return b}catch(b){b=v(b);if(b[1]===D[1]){e(D[34],a,c,0);return 0}throw b}}function
g(d,c,b){return t(d,c)?r(b,function(a){return u(c)}):a(b,0)}function
m(h){var
f=e(D[23],h,sm,eY),i=a(D[30],f),d=a(b[1],kr);function
n(f){for(;;){var
a=s(i);if(a){var
c=a[1];e(b[10],d,c[1],c[2]);continue}return 0}}try{g(0,f,n);a(l[82],i);var
o=e(D[23],h,sp,eY),p=[0,a(D[31],o),1,d];return p}catch(f){f=v(f);if(f===k){a(l[82],i);var
m=e(D[23],h,sn,eY),j=a(D[31],m);g(1,m,function(g){function
f(b,a){return e(d$[1],j,[0,b,a],so)}c(b[12],f,d);return a(l[51],j)});return[0,j,1,d]}throw f}}function
w(c){var
d=c[1],e=c[3];return 0===c[2]?0:(a(l[64],d),a(b[2],e),c[2]=0,0)}function
n(c,i,h){var
d=c[1],j=c[3];if(0===c[2])throw f;var
k=a(D[33],d);e(b[10],j,i,h);return g(1,k,function(b){e(d$[1],d,[0,i,h],sq);return a(l[51],d)})}function
o(a,d){var
e=a[3];if(0===a[2])throw f;return c(b[7],e,d)}return[0,q,m,o,n,w,function(c,e){var
b=[d,function(b){try{var
a=[0,m(c)];return a}catch(a){return 0}}];return function(c){var
f=i(b),g=j===f?b[1]:d===f?a(h[2],b):b;if(g){var
k=g[1];try{var
m=o(k,c);return m}catch(b){b=v(b);if(b===M){var
l=a(e,c);n(k,c,l);return l}throw b}}return a(e,c)}}]}];aU(703,cR,"Micromega_plugin.Persistent_cache");var
sr=0;function
ss(d,c,b){var
f=a(D[97],0)[1],g=a(c,b),h=a(D[97],0)[1]-f;e(n[2],st,d,h);a(l[51],l[27]);return g}var
eF=l[7],f5=[0,eF],eG=[0,1],f6=[0,eF];function
f7(a){return[0,eG[1],f6[1]]}function
eH(a){return f5[1]}function
iS(b,a){function
c(b){var
c=b?b[1]:eF;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,0,e(g[21],l[16],b,su),b,d,c]}function
sv(a){eG[1]=a;return 0}var
sy=[0,0,sx,sw,function(a){return eG[1]},sv],sA=iS(sz,f5);c(f8[3],0,sA);var
sC=iS(sB,f6);c(f8[3],0,sC);c(f8[4],0,sy);function
bq(d,b){if(typeof
b==="number")return 0===b?c(l[54],d,sD):c(l[54],d,sE);else
switch(b[0]){case
0:return c(l[54],d,sF);case
1:return x(n[1],d,sG,k[32][3],b[2]);case
2:return C(n[1],d,sH,bq,b[1],bq,b[2]);case
3:return C(n[1],d,sI,bq,b[1],bq,b[2]);case
4:return x(n[1],d,sJ,bq,b[1]);default:var
e=b[2],f=b[3],g=b[1],h=e?a(F[1][8],e[1]):sL;return d0(n[1],d,sK,bq,g,h,bq,f)}}function
aM(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
d=b[3],e=b[2];return[1,a(c,b[1]),e,d];case
2:var
f=b[1],g=aM(c,b[2]);return[2,aM(c,f),g];case
3:var
h=b[1],i=aM(c,b[2]);return[3,aM(c,h),i];case
4:return[4,aM(c,b[1])];default:var
j=b[2],k=b[1],l=aM(c,b[3]);return[5,aM(c,k),j,l]}}function
a3(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1],b[2],b[3]];case
2:var
d=b[1],e=a3(c,b[2]);return[2,a3(c,d),e];case
3:var
f=b[1],g=a3(c,b[2]);return[3,a3(c,f),g];case
4:return[4,a3(c,b[1])];default:var
h=b[2],i=b[1],j=a3(c,b[3]);return[5,a3(c,i),h,j]}}function
f9(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];if(b){var
c=b[1];return[0,c,f9(a[3])]}}return 0}var
eI=0,sM=0;function
eJ(I,H,m,k,b){function
i(b,a){return c(l[25],b,a)}function
j(b,e){if(e){var
h=e[2],d=e[1],i=c(k,b[1],d[1]);if(i){if(a(m,i[1]))return[0,[0,b[2],[0,d[2],0]]];var
f=j(b,h);return 0===f[0]?[0,f[1]]:[1,[0,d,f[1]]]}var
g=j(b,h);return 0===g[0]?[0,g[1]]:[1,[0,d,g[1]]]}var
l=c(k,b[1],b[1]);return l?a(m,l[1])?[0,[0,b[2],0]]:[1,[0,b,0]]:[1,[0,b,0]]}function
h(a,d){if(a){var
m=a[1],f=h(a[2],d),k=f[2],n=f[1],i=function(k,f){var
g=f[2],h=f[1],a=m,d=k;for(;;){if(a){var
i=a[2],e=j(a[1],d);if(0!==e[0]){var
a=i,d=e[1];continue}var
b=[0,e[1]]}else
var
b=[1,d];return 0===b[0]?[0,h,c(l[25],g,b[1])]:[0,[0,b[1],h],g]}},b=e(g[21],i,d,sN),o=b[1],p=c(l[25],k,b[2]);return[0,c(l[25],n,o),p]}return[0,eI,0]}function
f(O,N){var
b=O,d=N;for(;;)if(typeof
d==="number")return 0===d?b?[0,eI,0]:[0,cS,0]:b?[0,cS,0]:[0,eI,0];else
switch(d[0]){case
0:return b?[0,cS,0]:[0,cS,0];case
1:var
e=d[2],j=d[1],P=0;if(b)var
L=a(H,j),M=function(a){function
b(a){return[0,a,e]}return c(g[17],b,a)},k=c(g[17],M,L);else
var
J=a(I,j),K=function(a){function
b(a){return[0,a,e]}return c(g[17],b,a)},k=c(g[17],K,J);return[0,k,P];case
2:var
Q=d[2],m=f(b,d[1]),n=m[2],o=m[1],p=f(b,Q),q=p[2],r=p[1];if(b){var
R=c(l[25],n,q);return[0,i(o,r),R]}var
s=h(o,r),S=s[1],T=c(l[25],q,s[2]);return[0,S,c(l[25],n,T)];case
3:var
U=d[2],t=f(b,d[1]),u=t[2],v=t[1],w=f(b,U),x=w[2],y=w[1];if(b){var
z=h(v,y),V=z[1],W=c(l[25],x,z[2]);return[0,V,c(l[25],u,W)]}var
X=c(l[25],u,x);return[0,i(v,y),X];case
4:var
b=1-b,d=d[1];continue;default:var
Y=d[3],A=f(1-b,d[1]),B=A[2],C=A[1],D=f(b,Y),E=D[2],F=D[1];if(b){var
G=h(C,F),Z=G[1],_=c(l[25],E,G[2]);return[0,Z,c(l[25],B,_)]}var
$=c(l[25],B,E);return[0,i(C,F),$]}}return f(1,b)}var
ao=a(d9[1],[0,Z]),sO=a(bF[1],[0,Z]);function
iT(f,a){function
d(h,g){var
b=h,a=g;for(;;){if(a){var
e=a[2],i=a[1];if(c(ao[3],b,f))return[0,i,d(b+1|0,e)];var
b=b+1|0,a=e;continue}return 0}}return d(0,a)}var
sP=c(l[25],cT[6],f_),sQ=c(l[25],cT[5],sP),sR=c(l[25],[0,iU,0],sQ),iV=c(l[25],cT[7],sR);function
L(d,c,b){var
f=e(cT[4],d,c,b),g=a(sS[50],f);return a(m[8],g)}var
sT=cT[7];function
aj(a){return L(sU,sT,a)}function
t(a){return L(sV,iV,a)}function
aC(a){return L(sW,iW,a)}function
ak(a){return L(sX,iX,a)}function
aN(a){return L(sY,iY,a)}function
aO(a){return L(sZ,f_,a)}var
a4=[d,function(a){return aj(s0)}],a5=[d,function(a){return aj(s1)}],cU=[d,function(a){return aj(s2)}],cV=[d,function(a){return aj(s3)}],a6=[d,function(a){return aj(s4)}],ap=[d,function(a){return aj(s5)}],cW=[d,function(a){return t(s6)}],cX=[d,function(a){return t(s7)}],s9=[d,function(a){return t(s8)}],cY=[d,function(a){return aj(s_)}],cZ=[d,function(a){return aj(s$)}],tb=[d,function(a){return aj(ta)}],c0=[d,function(a){return aC(tc)}],c1=[d,function(a){return aC(td)}],c2=[d,function(a){return aj(te)}],tg=[d,function(a){return aj(tf)}],ti=[d,function(a){return aj(th)}],tk=[d,function(a){return aC(tj)}],a7=[d,function(a){return aC(tl)}],a8=[d,function(a){return aC(tm)}],a9=[d,function(a){return aC(tn)}],a_=[d,function(a){return aC(to)}],c3=[d,function(a){return aC(tp)}],c4=[d,function(a){return aC(tq)}],c5=[d,function(a){return aC(tr)}],c6=[d,function(a){return t(ts)}],a$=[d,function(a){return t(tt)}],tv=[d,function(a){return t(tu)}],aq=[d,function(a){return t(tw)}],ty=[d,function(a){return t(tx)}],c7=[d,function(a){return aO(tz)}],c8=[d,function(a){return aO(tA)}],c9=[d,function(a){return aO(tB)}],c_=[d,function(a){return aO(tC)}],c$=[d,function(a){return aO(tD)}],da=[d,function(a){return aO(tE)}],db=[d,function(a){return aO(tF)}],dc=[d,function(a){return aO(tG)}],dd=[d,function(a){return aO(tH)}],ar=[d,function(a){return t(tI)}],as=[d,function(a){return t(tJ)}],tL=[d,function(a){return t(tK)}],tN=[d,function(a){return t(tM)}],tP=[d,function(a){return t(tO)}],tR=[d,function(a){return t(tQ)}],tT=[d,function(a){return t(tS)}],iZ=[d,function(a){return aN(tU)}],i0=[d,function(a){return aN(tV)}],i1=[d,function(a){return aN(tW)}],i2=[d,function(a){return aN(tX)}],at=[d,function(a){return aj(tY)}],b3=[d,function(a){return aN(tZ)}],b4=[d,function(a){return aN(t0)}],b5=[d,function(a){return aN(t1)}],b6=[d,function(a){return aN(t2)}],b7=[d,function(a){return aN(t3)}],t5=[d,function(a){return t(t4)}],t7=[d,function(a){return t(t6)}],i3=[d,function(a){return t(t8)}],i4=[d,function(a){return t(t9)}],i5=[d,function(a){return t(t_)}],b8=[d,function(a){return t(t$)}],b9=[d,function(a){return t(ua)}],b_=[d,function(a){return t(ub)}],b$=[d,function(a){return t(uc)}],ca=[d,function(a){return t(ud)}],i6=[d,function(a){return ak(ue)}],i7=[d,function(a){return ak(uf)}],i8=[d,function(a){return ak(ug)}],i9=[d,function(a){return ak(uh)}],ac=[d,function(a){return ak(ui)}],aD=[d,function(a){return ak(uj)}],aP=[d,function(a){return ak(uk)}],ad=[d,function(a){return ak(ul)}],un=[d,function(a){return ak(um)}],ba=[d,function(a){return ak(uo)}],cb=[d,function(a){return ak(up)}],bb=[d,function(a){return ak(uq)}],bc=[d,function(a){return ak(ur)}],de=[d,function(a){return t(us)}],df=[d,function(a){return t(ut)}],dg=[d,function(a){return t(uu)}],dh=[d,function(a){return t(uv)}],di=[d,function(a){return t(uw)}],dj=[d,function(a){return t(ux)}],dk=[d,function(a){return t(uy)}],dl=[d,function(a){return t(uz)}],dm=[d,function(a){return t(uA)}],dn=[d,function(a){return t(uB)}],dp=[d,function(a){return t(uC)}],dq=[d,function(a){return t(uD)}],dr=[d,function(a){return t(uE)}],ds=[d,function(a){return t(uF)}],dt=[d,function(a){return t(uG)}],du=[d,function(a){return t(uH)}],dv=[d,function(a){return t(uI)}],dw=[d,function(a){return t(uJ)}],dx=[d,function(a){return t(uK)}],dy=[d,function(a){return t(uL)}],dz=[d,function(a){return t(uM)}],dA=[d,function(a){return t(uN)}],dB=[d,function(a){return t(uO)}],uQ=[d,function(a){return t(uP)}],uU=[d,function(a){return L(uT,uS,uR)}],uY=[d,function(a){return L(uX,uW,uV)}],dC=[d,function(a){return L(u1,u0,uZ)}],dD=[d,function(a){return L(u4,u3,u2)}],dE=[d,function(a){return L(u7,u6,u5)}],dF=[d,function(a){return L(u_,u9,u8)}],dG=[d,function(a){return L(vb,va,u$)}],dH=[d,function(a){return L(ve,vd,vc)}],dI=[d,function(a){return L(vh,vg,vf)}],dJ=[d,function(a){return L(vk,vj,vi)}],vo=[d,function(a){return L(vn,vm,vl)}],vs=[d,function(a){return L(vr,vq,vp)}],vw=[d,function(a){return L(vv,vu,vt)}],vA=[d,function(a){return L(vz,vy,vx)}],dK=[d,function(a){return L(vD,vC,vB)}],vH=[d,function(a){return L(vG,vF,vE)}];function
vI(b){if(typeof
b==="number")return vJ;else
switch(b[0]){case
0:return b[1];case
1:return a(l[21],b[1]);case
2:return vK;case
3:return b[1];default:return vL}}var
N=[aW,vM,aV(0)];function
cc(b,e){var
a=c(m[3],b,e);switch(a[0]){case
9:var
f=a[2],d=c(m[3],b,a[1]);if(12===d[0])return[0,d[1][1][2],f];throw N;case
12:return[0,a[1][1][2],[0]];default:throw N}}function
f$(a,d){var
b=cc(a,d),c=b[1],e=b[2];if(1===c)return 0;if(2===c)return[0,f$(a,y(e,0)[1])];throw N}function
i_(c,b){var
d=a(k[29][1],b);return e(n[1],c,vN,d)}function
eK(b){if(b){var
c=i(cZ),f=[0,eK(b[1])],g=j===c?cZ[1]:d===c?a(h[2],cZ):cZ;return a(m[21],[0,g,f])}var
e=i(cY);return j===e?cY[1]:d===e?a(h[2],cY):cY}function
cd(a,e){var
b=cc(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw N;switch(d){case
0:return[0,cd(a,y(c,0)[1])];case
1:return[1,cd(a,y(c,0)[1])];default:return 0}}function
au(b){if(typeof
b==="number"){var
c=i(a7);return j===c?a7[1]:d===c?a(h[2],a7):a7}else{if(0===b[0]){var
e=i(a9),g=[0,au(b[1])],k=j===e?a9[1]:d===e?a(h[2],a9):a9;return a(m[21],[0,k,g])}var
f=i(a8),l=[0,au(b[1])],n=j===f?a8[1]:d===f?a(h[2],a8):a8;return a(m[21],[0,n,l])}}function
dL(c,b){var
d=a(k[29][2],b);return e(n[1],c,vO,d)}function
i$(b){if(b){var
c=i(c1),f=[0,au(b[1])],g=j===c?c1[1]:d===c?a(h[2],c1):c1;return a(m[21],[0,g,f])}var
e=i(c0);return j===e?c0[1]:d===e?a(h[2],c0):c0}function
ga(b){if(typeof
b==="number"){var
c=i(a7);return j===c?a7[1]:d===c?a(h[2],a7):a7}else{if(0===b[0]){var
e=i(a9),g=[0,ga(b[1])],k=j===e?a9[1]:d===e?a(h[2],a9):a9;return a(m[21],[0,k,g])}var
f=i(a8),l=[0,ga(b[1])],n=j===f?a8[1]:d===f?a(h[2],a8):a8;return a(m[21],[0,n,l])}}function
vP(c,b){var
d=a(k[29][4],b);return e(n[1],c,vQ,d)}function
ja(d,b){var
e=a(k[29][3],b),f=a(l[21],e);return c(l[54],d,f)}function
vR(k,g,f,e,b){var
l=b[1],n=a(e,b[2]),c=i(c2),o=[0,k,g,a(f,l),n],p=j===c?c2[1]:d===c?a(h[2],c2):c2;return a(m[21],[0,p,o])}function
bL(a,e){var
b=cc(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw N;switch(d){case
0:return 0;case
1:return[0,cd(a,y(c,0)[1])];default:return[1,cd(a,y(c,0)[1])]}}function
br(b){if(typeof
b==="number"){var
c=i(c3);return j===c?c3[1]:d===c?a(h[2],c3):c3}else{if(0===b[0]){var
e=i(c4),g=[0,au(b[1])],k=j===e?c4[1]:d===e?a(h[2],c4):c4;return a(m[21],[0,k,g])}var
f=i(c5),l=[0,au(b[1])],n=j===f?c5[1]:d===f?a(h[2],c5):c5;return a(m[21],[0,n,l])}}function
jb(c,b){var
d=a(k[29][7],b),f=a(q[33],d);return e(n[1],c,vS,f)}function
vT(b){var
e=a(k[17],b),f=au(a(k[30][6],e)),g=a(k[18],b),c=i(aq),l=[0,br(a(k[30][7],g)),f],n=j===c?aq[1]:d===c?a(h[2],aq):aq;return a(m[21],[0,n,l])}function
eL(b){var
e=au(b[2]),c=i(aq),f=[0,br(b[1]),e],g=j===c?aq[1]:d===c?a(h[2],aq):aq;return a(m[21],[0,g,f])}function
dM(b,l){var
f=c(m[3],b,l);if(9===f[0]){var
g=f[2],k=i(aq),n=f[1],o=j===k?aq[1]:d===k?a(h[2],aq):aq;if(e(m[94],b,n,o)){var
p=cd(b,y(g,1)[2]);return[0,bL(b,y(g,0)[1]),p]}throw N}throw N}function
bd(b,a){if(typeof
a==="number")return 0===a?c(l[54],b,vU):c(l[54],b,vV);else
switch(a[0]){case
0:return c(l[54],b,vW);case
1:return jb(b,a[1]);case
2:return C(n[1],b,vX,bd,a[1],bd,a[2]);case
3:return C(n[1],b,vY,bd,a[1],bd,a[2]);case
4:return C(n[1],b,vZ,bd,a[1],bd,a[2]);case
5:return x(n[1],b,v0,bd,a[1]);default:return x(n[1],b,v1,bd,a[1])}}function
be(b){if(typeof
b==="number"){if(0===b){var
c=i(c7);return j===c?c7[1]:d===c?a(h[2],c7):c7}var
e=i(c8);return j===e?c8[1]:d===e?a(h[2],c8):c8}else
switch(b[0]){case
0:var
f=i(c9),q=[0,eL(b[1])],r=j===f?c9[1]:d===f?a(h[2],c9):c9;return a(m[21],[0,r,q]);case
1:var
g=i(c_),s=[0,br(b[1])],t=j===g?c_[1]:d===g?a(h[2],c_):c_;return a(m[21],[0,t,s]);case
2:var
u=b[1],v=be(b[2]),k=i(c$),w=[0,be(u),v],x=j===k?c$[1]:d===k?a(h[2],c$):c$;return a(m[21],[0,x,w]);case
3:var
y=b[1],z=be(b[2]),l=i(da),A=[0,be(y),z],B=j===l?da[1]:d===l?a(h[2],da):da;return a(m[21],[0,B,A]);case
4:var
C=b[1],D=be(b[2]),n=i(db),E=[0,be(C),D],F=j===n?db[1]:d===n?a(h[2],db):db;return a(m[21],[0,F,E]);case
5:var
o=i(dc),G=[0,be(b[1])],H=j===o?dc[1]:d===o?a(h[2],dc):dc;return a(m[21],[0,H,G]);default:var
p=i(dd),I=[0,be(b[1])],J=j===p?dd[1]:d===p?a(h[2],dd):dd;return a(m[21],[0,J,I])}}function
bf(a,e){var
c=cc(a,e),b=c[2],d=c[1]-1|0;if(7<d>>>0)throw N;switch(d){case
0:return 0;case
1:return 1;case
2:return[0,dM(a,y(b,0)[1])];case
3:var
f=bf(a,y(b,1)[2]);return[2,bf(a,y(b,0)[1]),f];case
4:var
g=bf(a,y(b,1)[2]);return[3,bf(a,y(b,0)[1]),g];case
5:var
h=bf(a,y(b,1)[2]);return[4,bf(a,y(b,0)[1]),h];case
6:return[5,bf(a,y(b,0)[1])];default:return[6,bf(a,y(b,0)[1])]}}function
jc(a,b,g){var
d=cc(a,g),e=d[2],f=d[1];if(1===f)return 0;if(2===f){var
h=jc(a,b,y(e,2)[3]);return[0,c(b,a,y(e,1)[2]),h]}throw N}function
jd(c,e,b){if(b){var
k=b[1],l=jd(c,e,b[2]),f=i(cW),n=[0,c,a(e,k),l],o=j===f?cW[1]:d===f?a(h[2],cW):cW;return a(m[21],[0,o,n])}var
g=i(cX),p=[0,c],q=j===g?cX[1]:d===g?a(h[2],cX):cX;return a(m[21],[0,q,p])}function
v2(f,e,b,d,a){function
c(d,a){if(a){var
e=a[2],f=a[1];return e?C(n[1],d,v3,b,f,c,e):x(n[1],d,v4,b,f)}return 0}return C(n[1],d,v5,f,c,a,e)}function
gb(e,d,a){function
b(d,a){switch(a[0]){case
0:return c(e,d,a[1]);case
1:return x(n[1],d,v6,dL,a[1]);case
2:return C(n[1],d,v7,b,a[1],b,a[2]);case
3:return C(n[1],d,v8,b,a[1],b,a[2]);case
4:return C(n[1],d,v9,b,a[1],b,a[2]);case
5:return x(n[1],d,v_,b,a[1]);default:return C(n[1],d,v$,b,a[1],ja,a[2])}}return b(d,a)}function
gc(e,q,b){function
c(b){switch(b[0]){case
0:var
f=i(df),r=[0,e,a(q,b[1])],s=j===f?df[1]:d===f?a(h[2],df):df;return a(m[21],[0,s,r]);case
1:var
g=i(de),t=[0,e,au(b[1])],u=j===g?de[1]:d===g?a(h[2],de):de;return a(m[21],[0,u,t]);case
2:var
v=b[1],w=c(b[2]),k=i(dg),x=[0,e,c(v),w],y=j===k?dg[1]:d===k?a(h[2],dg):dg;return a(m[21],[0,y,x]);case
3:var
z=b[1],A=c(b[2]),l=i(dj),B=[0,e,c(z),A],C=j===l?dj[1]:d===l?a(h[2],dj):dj;return a(m[21],[0,C,B]);case
4:var
D=b[1],E=c(b[2]),n=i(di),F=[0,e,c(D),E],G=j===n?di[1]:d===n?a(h[2],di):di;return a(m[21],[0,G,F]);case
5:var
o=i(dh),H=[0,e,c(b[1])],I=j===o?dh[1]:d===o?a(h[2],dh):dh;return a(m[21],[0,I,H]);default:var
J=b[1],K=i$(b[2]),p=i(dk),L=[0,e,c(J),K],M=j===p?dk[1]:d===p?a(h[2],dk):dk;return a(m[21],[0,M,L])}}return c(b)}function
gd(e,l,b){function
c(b){switch(b[0]){case
0:var
f=i(dm),n=[0,e,a(l,b[1])],o=j===f?dm[1]:d===f?a(h[2],dm):dm;return a(m[21],[0,o,n]);case
1:var
p=b[1],q=c(b[2]),g=i(dn),r=[0,e,au(p),q],s=j===g?dn[1]:d===g?a(h[2],dn):dn;return a(m[21],[0,s,r]);default:var
t=b[2],u=b[1],v=c(b[3]),w=au(t),k=i(dl),x=[0,e,c(u),w,v],y=j===k?dl[1]:d===k?a(h[2],dl):dl;return a(m[21],[0,y,x])}}return c(b)}function
eM(d,c,a){function
b(c,a){switch(a[0]){case
0:return x(n[1],c,wa,d,a[1]);case
1:return C(n[1],c,wb,dL,a[1],b,a[2]);default:return bO(n[1],c,wc,b,a[1],dL,a[2],b,a[3])}}return b(c,a)}function
wd(d,b,a){function
e(b,a){function
e(a){var
c=a[2],e=a[1][1],f=k[32][3];function
g(a,b){return eM(d,a,b)}return C(n[1],b,we,g,e,f,c)}return c(g[15],e,a)}function
f(a){return x(n[1],b,wf,e,a)}return c(g[15],f,a)}function
wg(b,f,k){var
g=i(b),c=j===g?b[1]:d===g?a(h[2],b):b;function
e(b){if(typeof
b==="number"){var
g=i(dB),r=[0,c],s=j===g?dB[1]:d===g?a(h[2],dB):dB;return a(m[21],[0,s,r])}else
switch(b[0]){case
0:var
k=i(dv),t=[0,c,eK(b[1])],u=j===k?dv[1]:d===k?a(h[2],dv):dv;return a(m[21],[0,u,t]);case
1:var
l=i(dw),v=[0,c,gd(c,f,b[1])],w=j===l?dw[1]:d===l?a(h[2],dw):dw;return a(m[21],[0,w,v]);case
2:var
x=b[1],y=e(b[2]),n=i(dy),z=[0,c,gd(c,f,x),y],A=j===n?dy[1]:d===n?a(h[2],dy):dy;return a(m[21],[0,A,z]);case
3:var
B=b[1],C=e(b[2]),o=i(dx),D=[0,c,e(B),C],E=j===o?dx[1]:d===o?a(h[2],dx):dx;return a(m[21],[0,E,D]);case
4:var
F=b[1],G=e(b[2]),p=i(dz),H=[0,c,e(F),G],I=j===p?dz[1]:d===p?a(h[2],dz):dz;return a(m[21],[0,I,H]);default:var
q=i(dA),J=[0,c,a(f,b[1])],K=j===q?dA[1]:d===q?a(h[2],dA):dA;return a(m[21],[0,K,J])}}return e(k)}function
wh(e,b,a){function
d(b,a){if(typeof
a==="number")return c(n[1],b,wi);else
switch(a[0]){case
0:return x(n[1],b,wj,i_,a[1]);case
1:var
f=a[1],g=function(a,b){return eM(e,a,b)};return x(n[1],b,wk,g,f);case
2:var
h=a[2],i=a[1],j=function(a,b){return eM(e,a,b)};return C(n[1],b,wl,j,i,d,h);case
3:return C(n[1],b,wm,d,a[1],d,a[2]);case
4:return C(n[1],b,wn,d,a[1],d,a[2]);default:return x(n[1],b,wo,e,a[1])}}return d(b,a)}function
je(l){switch(l){case
0:var
b=i(dp);return j===b?dp[1]:d===b?a(h[2],dp):dp;case
1:var
c=i(dq);return j===c?dq[1]:d===c?a(h[2],dq):dq;case
2:var
e=i(dr);return j===e?dr[1]:d===e?a(h[2],dr):dr;case
3:var
f=i(dt);return j===f?dt[1]:d===f?a(h[2],dt):dt;case
4:var
g=i(ds);return j===g?ds[1]:d===g?a(h[2],ds):ds;default:var
k=i(du);return j===k?du[1]:d===k?a(h[2],du):du}}function
jf(a,b){switch(b){case
0:return c(n[1],a,wp);case
1:return c(n[1],a,wq);case
2:return c(n[1],a,wr);case
3:return c(n[1],a,ws);case
4:return c(n[1],a,wt);default:return c(n[1],a,wu)}}function
wv(b,c,a){var
d=a[3],e=a[2],f=a[1];function
g(a,c){return gb(b,a,c)}function
h(a,c){return gb(b,a,c)}return bO(n[1],c,ww,h,f,jf,e,g,d)}function
wx(c,e,b){var
g=b[2],k=b[1],l=gc(c,e,b[3]),n=je(g),f=i(dK),o=[0,c,gc(c,e,k),n,l],p=j===f?dK[1]:d===f?a(h[2],dK):dK;return a(m[21],[0,p,o])}function
dN(k,f,b){try{var
l=function(g){var
b=g[1],c=i(b),l=j===c?b[1]:d===c?a(h[2],b):b;return e(m[94],k,f,l)},n=c(g[33],l,b)[2];return n}catch(a){a=v(a);if(a===M)throw N;throw a}}var
ge=[0,[0,iZ,5],[0,[0,i0,3],[0,[0,i2,4],[0,[0,i1,2],0]]]],gf=[0,[0,i6,5],[0,[0,i7,3],[0,[0,i9,4],[0,[0,i8,2],0]]]],gg=[0,[0,i4,4],[0,[0,i3,2],[0,[0,i5,0],0]]];function
gh(a,c,b){return aT(wy[79],0,a[1],a[2],c,b)}function
jg(n,k){var
b=k[2],f=k[1],g=n[2],o=c(m[3],g,f);switch(o[0]){case
10:var
r=y(b,1)[2],s=y(b,0)[1];return[0,dN(g,f,ge),s,r];case
11:if(0===o[1][1][2]){var
p=i(at),t=j===p?at[1]:d===p?a(h[2],at):at;if(e(m[94],g,f,t)){var
q=i(a_),u=j===q?a_[1]:d===q?a(h[2],a_):a_;if(gh(n,y(b,0)[1],u)){var
v=y(b,2)[3];return[0,0,y(b,1)[2],v]}}throw N}break}return a(l[2],wz)}function
jh(n,k){var
b=k[2],f=k[1],g=n[2],o=c(m[3],g,f);switch(o[0]){case
10:var
r=y(b,1)[2],s=y(b,0)[1];return[0,dN(g,f,gf),s,r];case
11:if(0===o[1][1][2]){var
p=i(at),t=j===p?at[1]:d===p?a(h[2],at):at;if(e(m[94],g,f,t)){var
q=i(a$),u=j===q?a$[1]:d===q?a(h[2],a$):a$;if(gh(n,y(b,0)[1],u)){var
v=y(b,2)[3];return[0,0,y(b,1)[2],v]}}throw N}break}return a(l[2],wA)}function
ji(c,a){var
b=a[2],d=a[1],e=y(b,1)[2],f=y(b,0)[1];return[0,dN(c[2],d,gg),f,e]}function
wB(b,a){return 12===c(m[3],b,a)[0]?1:0}function
jj(k,f,b){try{var
l=function(g){var
b=g[1],c=i(b),l=j===c?b[1]:d===c?a(h[2],b):b;return e(m[94],k,f,l)},n=c(g[33],l,b)[2];return n}catch(a){a=v(a);if(a===M)return wC;throw a}}function
wD(f,h,c){function
d(a,c,b){if(a){var
f=a[1],i=a[2];if(e(m[94],h,f,b))return[0,a,c];var
g=d(i,c+1|0,b);return[0,[0,f,g[1]],g[2]]}return[0,[0,b,0],c]}var
b=d(f,1,c),g=b[1];return[0,g,a(k[30][2],b[2])]}function
wE(f,d,c){var
a=f,b=1;for(;;){if(a){var
g=a[2];if(e(m[94],d,a[1],c))return b;var
a=g,b=b+1|0;continue}throw[0,jk,wF]}}var
wG=0,bM=[0,wD,wE,wG,function(a){return a}];function
eN(d,i,t,s,f,b){function
l(c,b){var
a=e(bM[1],c,d,b);return[0,[1,a[2]],a[1]]}function
g(b,f){function
u(f,e,a){var
h=a[2],b=g(f,a[1]),i=b[1],d=g(b[2],h),j=d[2];return[0,c(e,i,d[1]),j]}try{var
C=[0,[0,a(i,f)],b];return C}catch(i){i=v(i);if(i===N){var
k=c(m[3],d,f);if(9===k[0]){var
h=k[2],n=k[1];if(10===c(m[3],d,n)[0]){var
j=jj(d,n,s);if(typeof
j==="number"){if(0===j){var
o=g(b,y(h,0)[1]);return[0,[5,o[1]],o[2]]}try{var
q=g(b,y(h,0)[1]),w=q[2],x=q[1],z=[0,c(t,x,y(h,1)[2]),w];return z}catch(c){c=v(c);if(a(bn[20],c)){var
p=e(bM[1],b,d,f);return[0,[1,p[2]],p[1]]}throw c}}else{if(0===j[0]){var
A=j[1],B=y(h,1)[2];return u(b,A,[0,y(h,0)[1],B])}var
r=e(bM[1],b,d,f);return[0,[1,r[2]],r[1]]}}return l(b,f)}return l(b,f)}throw i}}return g(f,b)}var
wI=[0,[0,b5,0],[0,[0,b7,1],0]],wJ=[0,[0,b6,[0,function(b,a){return[4,b,a]}]],wI],wK=[0,[0,b4,[0,function(b,a){return[3,b,a]}]],wJ],jl=[0,[0,b3,[0,function(b,a){return[2,b,a]}]],wK],wL=[0,[0,b_,0],[0,[0,ca,1],0]],wM=[0,[0,b$,[0,function(b,a){return[4,b,a]}]],wL],wN=[0,[0,b9,[0,function(b,a){return[3,b,a]}]],wM],jm=[0,[0,b8,[0,function(b,a){return[2,b,a]}]],wN],wO=[0,[0,aP,0],[0,[0,cb,1],0]],wP=[0,[0,ad,[0,function(b,a){return[4,b,a]}]],wO],wQ=[0,[0,aD,[0,function(b,a){return[3,b,a]}]],wP],jn=[0,[0,ac,[0,function(b,a){return[2,b,a]}]],wQ],wR=0,wS=[0,[0,ad,function(b,a){return[4,b,a]}],wR],wT=[0,[0,aD,function(b,a){return[3,b,a]}],wS],jo=[0,[0,ac,function(b,a){return[2,b,a]}],wT];function
eO(b,k){var
l=c(m[3],b,k);switch(l[0]){case
9:var
f=l[2],g=l[1];try{var
A=dN(b,g,jo),B=eO(b,y(f,0)[1]),C=c(A,B,eO(b,y(f,1)[2]));return C}catch(k){k=v(k);if(k===N){var
n=i(ba),u=j===n?ba[1]:d===n?a(h[2],ba):ba;if(e(m[94],b,g,u)){var
o=eO(b,y(f,0)[1]),w=a(r[gJ],o);if(c(r[77],w,wU))throw N;return[5,o]}var
p=i(bc),x=j===p?bc[1]:d===p?a(h[2],bc):bc;if(e(m[94],b,g,x))return[0,dM(b,y(f,0)[1])];var
q=i(bb),z=j===q?bb[1]:d===q?a(h[2],bb):bb;if(e(m[94],b,g,z))return[1,bL(b,y(f,0)[1])];throw N}throw k}case
10:var
s=i(ar),D=j===s?ar[1]:d===s?a(h[2],ar):ar;if(e(m[94],b,k,D))return 0;var
t=i(as),E=j===t?as[1]:d===t?a(h[2],as):as;if(e(m[94],b,k,E))return 1;throw N;default:throw N}}function
jp(c,b){a(wH[6],0);return eO(c,b)}function
jq(b){function
c(e,d){var
c=bL(b,d);if(typeof
c!=="number"&&1===c[0])return wV;return[6,e,a(r[12][15],c)]}function
d(a){return bL(b,a)}return function(a,e){return eN(b,d,c,jl,a,e)}}function
jr(d){function
b(e,f){var
b=bL(d,f);if(typeof
b!=="number"&&1===b[0]){if(0===e[0])return[0,c(r[85],e[1],b)];a(l[30],wW);a(l[51],l[27]);throw N}return[6,e,a(r[12][15],b)]}function
e(a){return dM(d,a)}return function(a,c){return eN(d,e,b,jm,a,c)}}function
js(b){function
c(d,c){var
e=f$(b,c);return[6,d,a(r[7][1],e)]}function
d(a){return jp(b,a)}return function(a,e){return eN(b,d,c,jn,a,e)}}function
eP(o,h,n,k,g){var
b=g[2],d=c(m[3],b,k);if(9===d[0]){var
f=c(o,g,[0,d[1],d[2]]),p=f[3],q=f[1],i=e(h,b,n,f[2]),r=i[1],j=e(h,b,i[2],p);return[0,[0,r,q,j[1]],j[2]]}return a(l[2],wX)}function
wY(a,b,c){return eP(jg,jq,a,b,c)}function
wZ(a,b,c){return eP(ji,jr,a,b,c)}function
w0(a,b,c){return eP(jh,js,a,b,c)}function
bt(a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return 2;case
1:return[0,a[1]];case
2:var
b=a[1],c=bt(a[2]);return[1,bt(b),c];case
3:var
d=a[1],e=bt(a[2]);return[2,bt(d),e];case
4:return[3,bt(a[1])];default:var
f=a[1],g=bt(a[3]);return[4,bt(f),g]}}function
jt(b,a){return[2,b,a]}function
ju(b,a){return[3,b,a]}function
jv(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
jw(b,a){return[5,b,0,a]}function
dO(e,d,b,a){if(typeof
b!=="number"&&0===b[0])if(typeof
a!=="number"&&0===a[0])return[0,d];return c(e,b,a)}function
w1(o,p,n,g,f){var
l=o[2];function
L(f,d,c){try{var
b=e(p,f,c,o),g=b[2],h=b[1],i=[0,[1,h,d,c],g,a(k[32][2],d)];return i}catch(b){b=v(b);if(a(bn[20],b))return[0,[0,c],f,d];throw b}}function
b(k,g,f){var
n=c(m[3],l,f);switch(n[0]){case
6:var
F=n[3],Q=n[2];if(e(m[107][13],l,1,F)){var
r=b(k,g,Q),R=r[1],s=b(r[2],r[3],F),S=s[3],T=s[2];return[0,dO(jw,f,R,s[1]),T,S]}break;case
9:var
p=n[2],q=n[1],G=p.length-1;if(!(3<=G))switch(G){case
0:break;case
1:var
H=i(cU),U=p[1],V=j===H?cU[1]:d===H?a(h[2],cU):cU;if(e(m[94],l,q,V)){var
t=b(k,g,U);return[0,[4,t[1]],t[2],t[3]]}break;default:var
u=p[1],v=p[2],I=i(a4),W=j===I?a4[1]:d===I?a(h[2],a4):a4;if(e(m[94],l,q,W)){var
w=b(k,g,u),X=w[1],y=b(w[2],w[3],v),Y=y[3],Z=y[2];return[0,dO(jt,f,X,y[1]),Z,Y]}var
J=i(a5),_=j===J?a5[1]:d===J?a(h[2],a5):a5;if(e(m[94],l,q,_)){var
z=b(k,g,u),$=z[1],A=b(z[2],z[3],v),aa=A[3],ab=A[2];return[0,dO(ju,f,$,A[1]),ab,aa]}var
K=i(cV),ac=j===K?cV[1]:d===K?a(h[2],cV):cV;if(e(m[94],l,q,ac)){var
B=b(k,g,u),ad=B[1],C=b(B[2],B[3],v),ae=C[3],af=C[2];return[0,dO(jv,f,ad,C[1]),af,ae]}}return L(k,g,f)}var
D=i(a6),O=j===D?a6[1]:d===D?a(h[2],a6):a6;if(e(m[94],l,f,O))return[0,0,k,g];var
E=i(ap),P=j===E?ap[1]:d===E?a(h[2],ap):ap;if(e(m[94],l,f,P))return[0,1,k,g];var
M=x(w2[3],0,o[1],o[2],f);if(a(w3[8],M))return[0,[0,f],k,g];throw N}return b(n,g,f)}function
w4(c,r,b){function
e(b){if(typeof
b==="number"){if(0===b){var
f=i(dC),s=[0,c],t=j===f?dC[1]:d===f?a(h[2],dC):dC;return a(m[21],[0,t,s])}var
g=i(dD),u=[0,c],v=j===g?dD[1]:d===g?a(h[2],dD):dD;return a(m[21],[0,v,u])}else
switch(b[0]){case
0:var
k=i(dI),w=[0,c,b[1]],x=j===k?dI[1]:d===k?a(h[2],dI):dI;return a(m[21],[0,x,w]);case
1:var
l=i(dH),y=[0,c,a(r,b[1])],z=j===l?dH[1]:d===l?a(h[2],dH):dH;return a(m[21],[0,z,y]);case
2:var
A=b[1],B=e(b[2]),n=i(dE),C=[0,c,e(A),B],D=j===n?dE[1]:d===n?a(h[2],dE):dE;return a(m[21],[0,D,C]);case
3:var
E=b[1],F=e(b[2]),o=i(dF),G=[0,c,e(E),F],H=j===o?dF[1]:d===o?a(h[2],dF):dF;return a(m[21],[0,H,G]);case
4:var
p=i(dG),I=[0,c,e(b[1])],J=j===p?dG[1]:d===p?a(h[2],dG):dG;return a(m[21],[0,J,I]);default:var
K=b[1],L=e(b[3]),q=i(dJ),M=[0,c,e(K),L],N=j===q?dJ[1]:d===q?a(h[2],dJ):dJ;return a(m[21],[0,N,M])}}return e(b)}function
jx(h,a){function
d(j,i){var
b=j,a=i;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
0:return e(bM[1],b,h,a[1])[1];case
4:var
a=a[1];continue;case
5:var
g=a[3],f=a[1],c=1;break;case
1:var
c=0;break;default:var
g=a[2],f=a[1],c=1}if(c){var
b=d(b,f),a=g;continue}return b}}return d(0,a)}function
jy(b){function
d(e){var
b=e;for(;;)switch(b[0]){case
0:return ao[1];case
1:var
f=a(k[29][2],b[1]);return a(ao[5],f);case
5:case
6:var
b=b[1];continue;default:var
g=b[1],h=d(b[2]),i=d(g);return c(ao[7],i,h)}}function
e(l){var
a=l;for(;;){if(typeof
a==="number")var
b=0;else
switch(a[0]){case
1:var
f=a[1],i=f[1],j=d(f[3]),k=d(i);return c(ao[7],k,j);case
4:var
a=a[1];continue;case
5:var
h=a[3],g=a[1],b=1;break;case
0:var
b=0;break;default:var
h=a[2],g=a[1],b=1}if(b){var
m=e(h),n=e(g);return c(ao[7],n,m)}return ao[1]}}return e(b)}var
w5=[d,function(x){function
o(c){var
b=c[1],e=i(b),f=c[2],g=j===e?b[1]:d===e?a(h[2],b):b;return[0,f,g]}var
p=c(g[17],o,ge),b=i(b7);function
q(b){var
c=a(k[29][3],b);return br(a(k[30][5],c))}var
r=j===b?b7[1]:d===b?a(h[2],b7):b7,e=i(b6),s=j===e?b6[1]:d===e?a(h[2],b6):b6,f=i(b5),t=j===f?b5[1]:d===f?a(h[2],b5):b5,l=i(b4),u=j===l?b4[1]:d===l?a(h[2],b4):b4,m=i(b3),v=j===m?b3[1]:d===m?a(h[2],b3):b3,n=i(a_),w=j===n?a_[1]:d===n?a(h[2],a_):a_;return[0,w,br,v,u,t,s,r,q,p]}],w6=[d,function(x){function
o(c){var
b=c[1],e=i(b),f=c[2],g=j===e?b[1]:d===e?a(h[2],b):b;return[0,f,g]}var
p=c(g[17],o,gg),b=i(ca);function
q(b){var
c=a(k[29][3],b);return br(a(k[30][5],c))}var
r=j===b?ca[1]:d===b?a(h[2],ca):ca,e=i(b$),s=j===e?b$[1]:d===e?a(h[2],b$):b$,f=i(b_),t=j===f?b_[1]:d===f?a(h[2],b_):b_,l=i(b9),u=j===l?b9[1]:d===l?a(h[2],b9):b9,m=i(b8),v=j===m?b8[1]:d===m?a(h[2],b8):b8,n=i(c6),w=j===n?c6[1]:d===n?a(h[2],c6):c6;return[0,w,eL,v,u,t,s,r,q,p]}];function
jz(o){var
e=i(ad),p=j===e?ad[1]:d===e?a(h[2],ad):ad,f=i(ac),q=j===f?ac[1]:d===f?a(h[2],ac):ac,g=i(as),b=j===g?as[1]:d===g?a(h[2],as):as;function
k(c,b){return a(m[21],[0,q,[0,c,b]])}function
l(c,b){return a(m[21],[0,p,[0,c,b]])}var
n=k(b,b);function
c(a){return typeof
a==="number"?b:0===a[0]?k(b,l(n,c(a[1]))):l(n,c(a[1]))}return c(o)}function
w7(e){var
b=a(k[29][3],e);if(0===b){var
c=i(ar);return j===c?ar[1]:d===c?a(h[2],ar):ar}return jz(a(k[30][2],b))}function
aQ(b){if(typeof
b==="number"){if(0===b){var
c=i(ar);return j===c?ar[1]:d===c?a(h[2],ar):ar}var
e=i(as);return j===e?as[1]:d===e?a(h[2],as):as}else
switch(b[0]){case
0:var
f=i(bc),q=[0,eL(b[1])],r=j===f?bc[1]:d===f?a(h[2],bc):bc;return a(m[21],[0,r,q]);case
1:var
g=i(bb),s=[0,br(b[1])],t=j===g?bb[1]:d===g?a(h[2],bb):bb;return a(m[21],[0,t,s]);case
2:var
u=b[1],v=aQ(b[2]),k=i(ac),w=[0,aQ(u),v],x=j===k?ac[1]:d===k?a(h[2],ac):ac;return a(m[21],[0,x,w]);case
3:var
y=b[1],z=aQ(b[2]),l=i(aD),A=[0,aQ(y),z],B=j===l?aD[1]:d===l?a(h[2],aD):aD;return a(m[21],[0,B,A]);case
4:var
C=b[1],D=aQ(b[2]),n=i(ad),E=[0,aQ(C),D],F=j===n?ad[1]:d===n?a(h[2],ad):ad;return a(m[21],[0,F,E]);case
5:var
o=i(ba),G=[0,aQ(b[1])],H=j===o?ba[1]:d===o?a(h[2],ba):ba;return a(m[21],[0,H,G]);default:var
p=i(aP),I=[0,aQ(b[1])],J=j===p?aP[1]:d===p?a(h[2],aP):aP;return a(m[21],[0,J,I])}}var
w8=[d,function(x){function
o(c){var
b=c[1],e=i(b),f=c[2],g=j===e?b[1]:d===e?a(h[2],b):b;return[0,f,g]}var
p=c(g[17],o,gf),b=i(cb);function
q(b){var
c=a(k[29][3],b);return eK(a(k[30][1],c))}var
r=j===b?cb[1]:d===b?a(h[2],cb):cb,e=i(ad),s=j===e?ad[1]:d===e?a(h[2],ad):ad,f=i(aP),t=j===f?aP[1]:d===f?a(h[2],aP):aP,l=i(aD),u=j===l?aD[1]:d===l?a(h[2],aD):aD,m=i(ac),v=j===m?ac[1]:d===m?a(h[2],ac):ac,n=i(a$),w=j===n?a$[1]:d===n?a(h[2],a$):a$;return[0,w,aQ,v,u,t,s,r,q,p]}];function
gi(h,g,f){var
b=[0,h,g,f];for(;;){var
d=b[1];if(0===d)return b[3];var
c=b[2];if(c){var
e=c[1],i=c[2],b=[0,d-1|0,i,a(m[18],[0,e[1],e[2],b[3]])];continue}throw[0,am,w9]}}function
w_(p,f,b){var
u=jy(b),w=a(ao[21],u);function
x(b,a){return[0,a,b+1|0]}var
q=c(g[18],x,w),r=jx(p,b);function
y(b){var
d=f[1],e=c(n[4],w$,b[2]);return[0,a(F[1][6],e),d]}var
o=c(g[17],y,q);function
z(b,f){var
d=m[14],e=c(n[4],xa,b+1|0);return[0,a(F[1][6],e),d]}var
s=c(g[18],z,r);function
A(b,a){return[0,a[1],b[1]]}var
B=e(g[23],A,q,o);function
t(e,b){function
d(b){switch(b[0]){case
0:return a(f[2],b[1]);case
1:var
h=a(k[29][2],b[1]),i=e+c(g[38],h,q)|0;return a(m[9],i);case
2:var
j=b[1],l=d(b[2]),n=[0,d(j),l];return a(m[21],[0,f[3],n]);case
3:var
o=b[1],p=d(b[2]),r=[0,d(o),p];return a(m[21],[0,f[4],r]);case
4:var
s=b[1],t=d(b[2]),u=[0,d(s),t];return a(m[21],[0,f[6],u]);case
5:var
v=[0,d(b[1])];return a(m[21],[0,f[5],v]);default:var
w=b[1],x=a(f[8],b[2]),y=[0,d(w),x];return a(m[21],[0,f[7],y])}}return d(b)}function
C(b,k,e){try{var
p=[0,c(g[38],b,f[9]),[0,k,e]],q=a(m[21],p);return q}catch(b){b=v(b);if(b===M){var
l=i(at),n=[0,f[1],k,e],o=j===l?at[1]:d===l?a(h[2],at):at;return a(m[21],[0,o,n])}throw b}}function
l(g,f,b){if(typeof
b==="number"){if(0===b){var
n=i(a6);return j===n?a6[1]:d===n?a(h[2],a6):a6}var
o=i(ap);return j===o?ap[1]:d===o?a(h[2],ap):ap}else
switch(b[0]){case
0:var
y=g+e(bM[2],r,p,b[1])|0;return a(m[9],y);case
1:var
k=b[1],v=k[2],w=k[1],x=t(f,k[3]);return C(v,t(f,w),x);case
2:var
z=b[1],A=l(g,f,b[2]),q=i(a4),B=[0,l(g,f,z),A],D=j===q?a4[1]:d===q?a(h[2],a4):a4;return a(m[21],[0,D,B]);case
3:var
E=b[1],F=l(g,f,b[2]),s=i(a5),G=[0,l(g,f,E),F],H=j===s?a5[1]:d===s?a(h[2],a5):a5;return a(m[21],[0,H,G]);case
4:var
u=i(ap),I=b[1],J=j===u?ap[1]:d===u?a(h[2],ap):ap,K=l(g,f,I);return c(m[33],K,J);default:var
L=b[1],M=l(g+1|0,f+1|0,b[3]),N=l(g,f,L);return c(m[33],N,M)}}var
D=a(g[1],o),E=a(g[1],s),G=a3(function(b){var
d=e(bM[2],r,p,b),f=c(n[4],xb,d),g=a(F[1][6],f);return a(m[10],g)},b),H=a(g[9],B),I=a(g[9],s),J=l(a(g[1],o),0,b);function
K(a){return[0,[0,a[1]],a[2]]}var
L=gi(D,c(g[17],K,o),J);function
N(a){return[0,[0,a[1]],a[2]]}return[0,gi(E,c(g[17],N,s),L),I,H,G]}var
o=[0,iU,f_,iV,iW,iX,iY,L,aj,t,aC,ak,aN,aO,a4,a5,cU,cV,a6,ap,cW,cX,s9,cY,cZ,tb,c0,c1,c2,tg,ti,tk,a7,a8,a9,a_,c3,c4,c5,c6,a$,tv,aq,ty,c7,c8,c9,c_,c$,da,db,dc,dd,ar,as,tL,tN,tP,tR,tT,iZ,i0,i1,i2,at,b3,b4,b5,b6,b7,t5,t7,i3,i4,i5,b8,b9,b_,b$,ca,i6,i7,i8,i9,ac,aD,aP,ad,un,ba,cb,bb,bc,de,df,dg,dh,di,dj,dk,dl,dm,dn,dp,dq,dr,ds,dt,du,dv,dw,dx,dy,dz,dA,dB,uQ,uU,uY,dC,dD,dE,dF,dG,dH,dI,dJ,vo,vs,vw,vA,dK,vH,vI,N,cc,f$,i_,eK,cd,au,dL,i$,ga,vP,ja,vR,bL,br,jb,vT,eL,dM,bd,be,bf,jc,jd,v2,dL,au,gb,gc,gd,eM,wd,wg,wh,je,jf,wv,wx,dN,ge,gf,gg,gh,jg,jh,ji,wB,jj,bM,eN,jl,jm,jn,bL,dM,jo,jp,jq,jr,js,eP,wY,wZ,w0,bt,jt,ju,jv,jw,dO,w1,w4,jx,jy,w5,w6,jz,w7,aQ,w8,gi,w_,function(f,e){var
c=e,b=f;for(;;){if(b){var
d=b[1],g=b[2],h=d[3],i=d[2],j=a(F[1][6],d[1]),c=x(m[40],j,i,h,c),b=g;continue}return c}}];function
dP(d){var
b=d;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:return[0,a(k[29][1],b[1]),0];case
2:var
b=b[2];continue;case
3:var
e=b[1],f=dP(b[2]),g=dP(e);return c(l[25],g,f);case
4:var
h=b[1],i=dP(b[2]),j=dP(h);return c(l[25],j,i)}return 0}}function
xc(a,f,e){return function(h){var
a=h;for(;;){if(a){var
d=a[1],i=a[2];try{var
j=c(g[7],e,d),k=c(g[7],f,d)===j?1:0,b=k}catch(a){a=v(a);if(a[1]!==jk)throw a;var
b=0}if(b){var
a=i;continue}return b}return 1}}(a)}function
xd(d,b,f){function
e(i,h){var
d=i,b=h;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
j=a(k[29][1],b[1]),l=c(g[7],f,j)[2];return c(F[1][10][7],d,l);case
2:var
b=b[2];continue;case
3:case
4:var
m=b[2],d=e(d,b[1]),b=m;continue}return d}}return e(d,b)}function
jA(e,d,a){if(a){var
g=a[2],b=c(k[6],e,[0,a[1],d]);if(b){var
h=b[1],f=jA(e,d,g);return f?[0,[0,h,f[1]]]:0}return 0}return xe}function
xf(e,d){var
c=e,b=d;for(;;){if(b){var
f=b[2],g=[0,c,[0,a(jB[2],b[1])]],c=a(jB[13],g),b=f;continue}return c}}var
dQ=[d,function(a){return e(o[7],xi,xh,xg)}],dR=[d,function(a){return e(o[7],xl,xk,xj)}],dS=[d,function(a){return e(o[7],xo,xn,xm)}],dT=[d,function(a){return e(o[7],xr,xq,xp)}];function
dU(c,b){if(typeof
b==="number"){var
e=i(dS),k=[0,c],l=j===e?dS[1]:d===e?a(h[2],dS):dS;return a(m[21],[0,l,k])}else{if(0===b[0]){var
f=i(dR),n=[0,c,b[1]],o=j===f?dR[1]:d===f?a(h[2],dR):dR;return a(m[21],[0,o,n])}var
p=b[2],q=b[1],r=dU(c,b[3]),g=i(dQ),s=[0,c,dU(c,q),p,r],t=j===g?dQ[1]:d===g?a(h[2],dQ):dQ;return a(m[21],[0,t,s])}}function
gj(b){if(b){var
c=b[1][1],d=0,f=function(d,b){var
e=b[1],f=a(k[30][2],b[2]);return x(r[88],c,f,e,d)};return e(g[20],f,d,b)}return 0}function
gk(b,a){return typeof
a==="number"?c(l[54],b,xs):0===a[0]?x(n[1],b,xt,o[aX],a[1]):bO(n[1],b,xu,gk,a[1],o[aX],a[2],gk,a[3])}function
eQ(b){if(typeof
b==="number"){var
c=o[56],n=i(c);return j===n?c[1]:d===n?a(h[2],c):c}else
switch(b[0]){case
0:var
t=b[1],u=eQ(b[2]),v=[0,e(o[bP],o[35],o[d4],t),u],f=o[57],p=i(f),w=j===p?f[1]:d===p?a(h[2],f):f;return a(m[21],[0,w,v]);case
1:var
x=b[1],y=eQ(b[2]),z=[0,e(o[bP],o[35],o[d4],x),y],g=o[58],q=i(g),A=j===q?g[1]:d===q?a(h[2],g):g;return a(m[21],[0,A,z]);default:var
k=o[55],r=i(k),B=b[3],C=b[2],D=b[1],E=j===r?k[1]:d===r?a(h[2],k):k,F=e(o[kU],E,eQ,B),G=e(o[bP],o[35],o[d4],C),H=[0,e(o[bP],o[35],o[d4],D),G,F],l=o[59],s=i(l),I=j===s?l[1]:d===s?a(h[2],l):l;return a(m[21],[0,I,H])}}function
bu(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:return 1;case
1:return 1;case
2:return 1+bu(a[2])|0;case
5:return 1;default:var
b=a[1],c=bu(a[2]);return bu(b)+c|0}}function
eR(a){if(typeof
a==="number")return 1;else
switch(a[0]){case
0:var
b=a[2],c=bu(a[1]);return eR(b)+c|0;case
1:var
d=a[2],f=bu(a[1]);return eR(d)+f|0;default:var
h=a[3],i=a[2],j=a[1],k=0,l=function(b,a){return eR(a)+b|0},m=e(g[20],l,k,h),n=bu(i);return(bu(j)+n|0)+m|0}}function
jC(a){return eQ(a)}function
aE(b,a){return C(n[1],b,xv,o[aX],a[1],o[141],a[2])}function
bN(d,b){if(typeof
b==="number")return c(n[1],d,xw);else
switch(b[0]){case
0:var
f=b[2],g=b[1],h=a(o[bi],o[aX]);return C(n[1],d,xx,h,g,bN,f);case
1:var
i=b[2],j=b[1],k=a(o[bi],o[aX]);return C(n[1],d,xy,k,j,bN,i);default:var
l=b[3],m=b[2],p=b[1],q=e(o[158],xA,xz,bN),r=a(o[bi],o[aX]),s=a(o[bi],o[aX]);return bO(n[1],d,xB,s,p,r,m,q,l)}}function
gl(h,g,f,e,b){if(b){var
i=b[1],m=i[2],n=i[1],c=gl(h,g,f,e,b[2]),j=c[3],k=c[2],l=c[1];try{var
d=aT(o[kQ],h,g,k,j,m),p=[0,[0,[0,n,d[1]],l],d[2],d[3]];return p}catch(b){b=v(b);if(a(bn[20],b))return[0,l,k,j];throw b}}return[0,0,f,e]}function
gm(d,c,h,g,f){var
i=a(k[32][1],0),b=aT(o[kQ],d,c,h,i,f),j=b[1],e=gl(d,c,b[2],b[3],g);return[0,e[1],j,e[2]]}var
dV=[d,function(q){var
b=o[55],f=i(b),l=j===f?b[1]:d===f?a(h[2],b):b,c=o[35],g=i(c),m=o[d4],n=j===g?c[1]:d===g?a(h[2],c):c,e=o[35],k=i(e),p=j===k?e[1]:d===k?a(h[2],e):e;return[0,p,n,m,l,jC]}],dW=[d,function(s){var
m=c(o[bP],o[39],o[d1]),b=o[e8],g=i(b),n=j===g?b[1]:d===g?a(h[2],b):b,e=o[39],k=i(e),p=o[d1],q=j===k?e[1]:d===k?a(h[2],e):e,f=o[39],l=i(f),r=j===l?f[1]:d===l?a(h[2],f):f;return[0,r,q,p,n,m]}],xC=[d,function(s){var
m=c(o[bP],o[39],o[d1]),b=o[e8],g=i(b),n=j===g?b[1]:d===g?a(h[2],b):b,e=o[43],k=i(e),p=o[kR],q=j===k?e[1]:d===k?a(h[2],e):e,f=o[40],l=i(f),r=j===l?f[1]:d===l?a(h[2],f):f;return[0,r,q,p,n,m]}];function
jD(d,b,a){return c(d,b,a)?1:c(d,a,b)?0:1}function
jE(d,b,a){function
e(b){return jD(d,a,b)}return c(g[27],e,b)}function
jF(h,g){var
d=0,b=g;for(;;){if(b){var
f=b[2],e=b[1];if(a(h,e))return[0,[0,e],c(l[25],d,f)];var
d=[0,e,d],b=f;continue}return[0,0,d]}}function
jG(b,a){return jF(function(c){return jE(b,a,c)},a)}function
gn(a,d){var
b=jG(a,d),c=b[1];if(c){var
e=c[1];return[0,e,gn(a,b[2])]}return 0}function
xD(b){return gn(function(c,b){var
d=a(m[8],b),f=a(m[8],c);return e(xF[37],xE[16],f,d)},b)}function
jH(b,q,p,n,l){var
f=o[kg],g=i(f),r=[0,b[2]],s=j===g?f[1]:d===g?a(h[2],f):f,k=a(m[21],[0,s,r]),t=c(o[k7],b[2],b[3]),u=e(o[lc],k,t,l),v=gj(n),w=dU(b[1],v);function
x(l){var
r=a(aR[42][6],l),f=i(dT),n=0,s=[0,[0,xG,q,p],0],t=[0,b[1]],v=j===f?dT[1]:d===f?a(h[2],dT):dT,x=[0,[0,xH,w,a(m[21],[0,v,t])],s],e=o[kn],g=i(e),y=[0,k],z=j===g?e[1]:d===g?a(h[2],e):e,A=[0,[0,xI,u,a(m[21],[0,z,y])],x],B=c(o[ke],A,r),C=[0,a(al[53],B),n];return a(I[66][20],C)}return a(dX[66][9],x)}function
jI(d,b){function
e(b){var
c=b[1];return[0,function(d){var
e=[0,a(b[2],0),d],c=a(b[3],e);return c?[0,[0,c[1],b]]:0},c]}var
f=c(g[17],e,d);function
h(a){return a[1]}var
i=c(g[17],h,b);return c(k[6],f,i)}function
eS(e,a){function
b(a){if(a){var
f=a[2],c=jI(e,a[1]);if(c){var
g=c[1],d=b(f);return d?[0,[0,g,d[1]]]:0}return 0}return xJ}return b(a)}function
xK(d,a,b){c(l[54],a,xL);function
e(b){return x(n[1],a,xM,d,b)}c(g[15],e,b);return c(l[54],a,xN)}function
jJ(f,d,b){function
h(j,b,i){var
d=b[2],m=b[1];function
n(b,a){return[0,b[1],a]}var
e=c(k[23],n,i);function
o(b){try{var
f=c(g[7],j,b)[1],d=f}catch(b){b=v(b);if(b[1]!==e_)throw b;var
d=a(l[2],xO)}return c(g[38],d,e)}try{var
t=c(d[5],m,o),h=t}catch(b){b=v(b);if(!a(bn[20],b))throw b;var
p=function(a){return a[1]},q=c(g[17],p,e),r=[0,a(d[2],0),q],f=a(d[3],r),s=f?f[1]:a(l[2],xP),h=s}return h}var
i=c(g[47],f,d);function
j(b){function
f(c){var
d=c[2],g=c[1],f=iT(a(d[2][4],d[1]),g);return e(k[11],V,f,b)}var
d=c(g[33],f,i);return h(d[1],d[2],b)}return c(g[17],j,b)}function
jK(B,b){function
e(b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
v=b[3],w=b[2],C=b[1];return c(k[33][3],w,B)?[1,C,w,v]:[0,v];case
2:var
D=b[2],g=e(b[1]),l=e(D);if(typeof
g!=="number"&&0===g[0])if(typeof
l!=="number"&&0===l[0]){var
n=o[14],x=i(n),E=[0,g[1],l[1]],F=j===x?n[1]:d===x?a(h[2],n):n;return[0,a(m[21],[0,F,E])]}return[2,g,l];case
3:var
G=b[2],p=e(b[1]),q=e(G);if(typeof
p!=="number"&&0===p[0])if(typeof
q!=="number"&&0===q[0]){var
r=o[15],y=i(r),H=[0,p[1],q[1]],I=j===y?r[1]:d===y?a(h[2],r):r;return[0,a(m[21],[0,I,H])]}return[3,p,q];case
4:var
s=e(b[1]);if(typeof
s!=="number"&&0===s[0]){var
t=o[16],z=i(t),J=[0,s[1]],K=j===z?t[1]:d===z?a(h[2],t):t;return[0,a(m[21],[0,K,J])]}return[4,s];default:var
A=b[2],L=b[3],u=e(b[1]),f=e(L);if(typeof
u!=="number"&&0===u[0]){var
M=u[1];if(A)return f;if(typeof
f!=="number"&&0===f[0])return[0,c(m[33],M,f[1])]}return[5,u,A,f]}}return e(b)}function
bg(c,b){if(typeof
c==="number"){if(0===c){if(typeof
b==="number")if(0===b)return 0}else
if(typeof
b==="number")if(0!==b)return 1}else
switch(c[0]){case
0:return[0,c[1]];case
1:if(typeof
b!=="number"&&1===b[0])return b;break;case
2:if(typeof
b!=="number"&&2===b[0]){var
d=b[1],e=c[1],f=bg(c[2],b[2]);return[2,bg(e,d),f]}break;case
3:if(typeof
b!=="number"&&3===b[0]){var
g=b[1],h=c[1],i=bg(c[2],b[2]);return[3,bg(h,g),i]}break;case
4:if(typeof
b!=="number"&&4===b[0])return[4,bg(c[1],b[1])];break;default:if(typeof
b!=="number"&&5===b[0]){var
j=b[2],k=b[1],m=c[1],n=bg(c[3],b[3]);return[5,bg(m,k),j,n]}}return a(l[2],xQ)}var
eT=[aW,xR,aV(0)];function
go(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return e(g[21],d,b,c)}function
gp(l,j,i,h,f,t,D,s,r,C){var
m=go(s,r)[1],n=eJ(l,j,i,h,m),b=n[1],u=n[2],p=eS(t,b);if(p){var
q=p[1],v=c(g[47],b,q),w=k[33][1],x=function(b,a){return c(k[33][4],a,b)},y=e(g[20],x,w,u),z=function(f,b){var
d=b[2],h=b[1],i=k[33][1],j=a(d[2][4],d[1]);function
l(b,a){var
d=c(g[7],h,b)[2];return c(k[33][4],d,a)}var
m=e(ao[15],l,j,i);return c(k[33][7],f,m)},d=jK(e(g[20],z,y,v),m),A=jJ(b,q,eJ(l,j,i,h,d)[1]),B=f9(d);return[0,[0,B,d,e(o[kU],f[4],f[5],A)]]}return 0}function
gq(d,c,b){var
f=a(dX[66][5],b);return e(al[13],d,c,f)}function
bh(aH,aG,aF,aE,aD,p,n){return function(aI,aJ){function
b(b){var
q=a(aR[42][4],b),K=a(aR[42][6],b),L=a(aR[42][14],b);try{var
w=[0,a(aR[42][5],b),q],r=gm(w,aH,o[e2][3],L,K),S=r[2],T=r[1],y=a(o[e2][4],r[3]),z=i(p),s=j===z?p[1]:d===z?a(h[2],p):p,A=i(n),U=j===A?n[1]:d===A?a(h[2],n):n,C=gp(aG,aF,aE,aD,s,aI,y,T,S,w);if(C)var
t=C[1],D=t[2],V=t[3],W=t[1],f=e(o[j5],q,U,D),u=f[3],X=f[4],Y=f[2],Z=f[1],E=function(a){return c(al[2],0,a[1])},_=c(g[17],E,u),$=a(I[66][20],_),aa=c(g[17],E,Y),ab=a(I[66][20],aa),ac=function(a){return[0,c(jL[1],0,[1,[0,a]])]},ad=a(F[1][6],xY),G=gq(F[1][10][1],ad,b),ae=function(b){var
c=b[2];return[0,a(m[10],b[1]),c]},af=c(g[17],ae,u),k=o[22],H=i(k),ag=0,ah=[0,s[4]],ai=j===H?k[1]:d===H?a(h[2],k):k,aj=[0,ab,[0,$,[0,jH(s,V,a(m[21],[0,ai,ah]),af,X),ag]]],ak=a(I[66][20],aj),am=c(o[ko],q,D),an=a(g[9],am),ao=function(a){return c(g[7],y,a[2]-1|0)},ap=c(g[17],ao,u),aq=c(l[25],an,ap),ar=c(I[66][3],ak,aJ),as=a(al[78],0),at=c(I[66][3],as,ar),au=[0,a(m[10],G),aq],av=a(m[34],au),aw=[0,a(al[45],av),0],ax=c(g[17],m[10],W),ay=[0,a(al[kl],ax),aw],az=[0,at,[0,a(I[66][20],ay),0]],aA=ac(G),aB=x(al[kx],1,xZ,aA,Z),J=c(I[66][19],aB,az);else
var
aC=a(bs[3],x0),J=c(I[66][4],0,aC);return J}catch(b){b=v(b);if(b===o[j_]){var
M=a(bs[3],xS);return c(I[66][4],0,M)}if(b===B[23]){var
N=a(bs[3],xT);return c(I[66][4],0,N)}if(b===eT){a(l[51],l[27]);var
O=c(l[16],xV,xU),P=c(l[16],xW,O),Q=c(l[16],xX,P),R=a(bs[3],Q);return c(I[66][4],0,R)}throw b}}return a(dX[66][9],b)}}function
jM(y,x,w){var
b=o[43],n=i(b),p=j===n?b[1]:d===n?a(h[2],b):b,f=o[40],q=i(f),z=o[kR],r=j===q?f[1]:d===q?a(h[2],f):f,g=o[e8],s=i(g),A=j===s?g[1]:d===s?a(h[2],g):g,k=o[22],t=i(k),B=[0,A],C=j===t?k[1]:d===t?a(h[2],k):k,D=a(m[21],[0,C,B]),l=o[kg],u=i(l),E=[0,p],F=j===u?l[1]:d===u?a(h[2],l):l,v=a(m[21],[0,F,E]),G=c(o[k7],p,z),H=e(o[lc],v,G,w),J=dU(r,gj(x));function
K(g){var
l=a(aR[42][6],g),n=[0,e(o[7],x4,x3,x2),[0,r]],p=[0,[0,x5,J,a(m[21],n)],[0,[0,x1,y,D],0]],b=o[kn],f=i(b),k=0,q=[0,v],s=j===f?b[1]:d===f?a(h[2],b):b,t=[0,[0,x6,H,a(m[21],[0,s,q])],p],u=c(o[ke],t,l),w=[0,a(al[53],u),k];return a(I[66][20],w)}return a(dX[66][9],K)}function
dY(aJ){return function(aK){var
L=o[197],M=r[e4],N=r[119],O=r[121],P=r[122],f=[d,function(s){var
m=c(o[bP],o[39],o[d1]),b=o[e8],g=i(b),n=j===g?b[1]:d===g?a(h[2],b):b,e=o[43],k=i(e),p=o[d1],q=j===k?e[1]:d===k?a(h[2],e):e,f=o[40],l=i(f),r=j===l?f[1]:d===l?a(h[2],f):f;return[0,r,q,p,n,m]}];function
b(b){var
p=a(aR[42][4],b),Q=a(aR[42][6],b),R=a(aR[42][14],b);try{var
u=[0,a(aR[42][5],b),p],q=gm(u,L,o[e2][3],R,Q),w=q[2],y=q[1],z=a(o[e2][4],q[3]),A=i(f),Y=j===A?f[1]:d===A?a(h[2],f):f,Z=function(b){var
c=b[2],d=b[1];return[0,d,aM(a(r[73],r[gJ]),c)]},_=c(g[17],Z,y),C=gp(M,N,O,P,Y,aJ,z,_,aM(a(r[73],r[gJ]),w),u);if(C)var
s=C[1],$=s[3],aa=s[2],ab=s[1],ac=function(a){return c(g[31],a[1],ab)},D=go(c(g[35],ac,y),w),ad=D[2],E=bg(aa,D[1]),k=o[213],G=i(k),ae=j===G?k[1]:d===G?a(h[2],k):k,n=e(o[j5],p,ae,E),t=n[3],af=n[4],ag=n[2],ah=n[1],H=function(a){return c(al[2],0,a[1])},ai=c(g[17],H,t),aj=a(I[66][20],ai),ak=c(g[17],H,ag),am=a(I[66][20],ak),an=function(a){return[0,c(jL[1],0,[1,[0,a]])]},ao=a(F[1][6],yb),J=gq(F[1][10][1],ao,b),ap=function(b){var
c=b[2];return[0,a(m[10],b[1]),c]},aq=[0,am,[0,aj,[0,jM($,c(g[17],ap,t),af),0]]],ar=a(I[66][20],aq),as=c(o[ko],p,E),at=a(g[9],as),au=function(a){return c(g[7],z,a[2]-1|0)},av=c(g[17],au,t),aw=c(l[25],at,av),ax=c(I[66][3],ar,aK),ay=a(al[78],0),az=c(I[66][3],ay,ax),aA=[0,a(m[10],J),aw],aB=a(m[34],aA),aC=[0,a(al[45],aB),0],aD=c(g[17],m[10],ad),aE=[0,a(al[kl],aD),aC],aF=[0,az,[0,a(I[66][20],aE),0]],aG=an(J),aH=x(al[kx],1,yc,aG,ah),K=c(I[66][19],aH,aF);else
var
aI=a(bs[3],yd),K=c(I[66][4],0,aI);return K}catch(b){b=v(b);if(b===o[j_]){var
S=a(bs[3],x7);return c(I[66][4],0,S)}if(b===B[23]){var
T=a(bs[3],x8);return c(I[66][4],0,T)}if(b===eT){a(l[51],l[27]);var
U=c(l[16],x_,x9),V=c(l[16],x$,U),W=c(l[16],ya,V),X=a(bs[3],W);return c(I[66][4],0,X)}throw b}}return a(dX[66][9],b)}}function
jN(d,c){var
b=a(d,c);return b?[0,[0,b[1],0]]:0}var
jO=a(cR[1],[0,V,bj[21]]),gr=a(yf[8],ye)?0:[d,function(a){throw eT}];function
jQ(o,n){var
f=i(gr);if(j!==f)if(d===f)a(h[2],gr);var
p=[0,yj,[0,yi,[0,c(l[16],yh,yg[36]),0]]],q=a(yk[3],0),m=e(g[20],yl[4],q,p),b=e(k[35],m,[0,m],[0,o,n]);return 0===b[0]?b[1]:a(l[2],b[1])}function
ym(a){return jQ(a[1],a[2])}var
jR=c(jO[6],jP,ym);function
gs(c,b){return a(jR,[0,c,b])}function
dZ(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,dZ(a[2])];default:var
c=a[2],d=a[1],e=dZ(a[3]);return[2,dZ(d),c,e]}}function
gt(f,b){var
d=gs(f,b);if(d){var
e=a(aB[51],d[1]);return c(r[111],b,e)?[0,e]:(a(l[30],yn),0)}return 0}function
jS(f,b){function
h(a){var
b=a[2];return[0,dZ(a[1]),b]}var
d=gs(f,c(g[17],h,b));if(d){var
e=a(aB[54],d[1]);return c(r[89],b,e)?[0,e]:(a(l[30],yo),a(l[51],l[27]),0)}return 0}function
ce(e,d,b){function
f(i,h){var
b=i,d=h;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
g=a(k[29][1],b[1]);return e<=g?c(ao[4],g-e|0,d):d;case
2:var
b=b[2];continue;case
3:case
4:var
j=b[1],l=f(b[2],d),b=j,d=l;continue}return d}}return f(b,d)}function
cf(a){return ce(0,ao[1],a)}function
aS(b,d){function
c(b){if(typeof
b!=="number")switch(b[0]){case
0:var
e=a(d,a(k[29][1],b[1]));return[0,a(k[30][1],e)];case
2:var
f=b[1];return[2,f,c(b[2])];case
3:var
g=b[1],h=c(b[2]);return[3,c(g),h];case
4:var
i=b[1],j=c(b[2]);return[4,c(i),j]}return b}return c(b)}function
eU(a){function
d(i,h,f){var
b=i,a=h,c=f;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=ce(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=ce(b,c,a[1]),b=b+1|0,a=l,c=m;continue;default:var
n=a[3],o=a[1],p=ce(b,ce(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return e(g[20],q,p,n)}}return d(0,a,ao[1])}function
eV(b,f){function
d(c,b){return b<c?b:a(f,b-c|0)+c|0}function
e(b,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
f=a[1],g=e(b+1|0,a[2]);return[0,aS(f,function(a){return d(b,a)}),g];case
1:var
h=a[1],i=e(b+1|0,a[2]);return[1,aS(h,function(a){return d(b,a)}),i];default:var
j=a[3],k=a[2],l=a[1],m=function(a){return e(b+1|0,a)},n=c(r[10],m,j),o=aS(k,function(a){return d(b,a)});return[2,aS(l,function(a){return d(b,a)}),o,n]}}return e(0,b)}function
cg(d,b){function
e(b){var
c=b[2];return[0,a(r[71],b[1]),c]}return a(d,c(g[17],e,b))}var
gu=a(cR[1],[0,V,bj[21]]),jT=a(cR[1],[0,V,bj[21]]);function
yp(a){var
b=a[1],d=a[2];return cg(c(aB[91],b[1],b[2]),d)}var
jU=c(gu[6],yq,yp);function
yr(a){var
b=a[1],d=a[2];return cg(c(aB[92],b[1],b[2]),d)}var
jV=c(gu[6],ys,yr);function
yt(b){var
c=b[2];return cg(a(aB[30],b[1]),c)}var
jW=c(jT[6],yu,yt);function
yv(b,a){return e(o[bw],aE,b,a[1])}var
yw=a(o[bi],aE),jX=[0,yx,eH,function(a){var
b=a[2];return cg(c(aB[29],a[1],aB[5]),b)},cf,aS,yw,yv];function
yy(b,a){return e(o[bw],aE,b,a[1])}var
yz=a(o[bi],aE),jY=[0,yA,eH,function(a){var
b=a[2];return cg(c(aB[29],a[1],aB[5]),b)},cf,aS,yz,yy];function
yB(b,a){return e(o[bw],aE,b,a[1])}var
gv=[0,yC,eH,jW,cf,aS,a(o[bi],aE),yB];function
gw(c,b){function
d(b,a){return e(o[bw],aE,b,a[1])}var
f=a(o[bi],aE);function
g(a){return gt(a[1],a[2])}return[0,yD,function(a){return[0,c,b]},g,cf,aS,f,d]}function
gx(c,b){function
d(b,a){return e(o[bw],aE,b,a[1])}var
f=a(o[bi],aE);function
g(a){return gt(a[1],a[2])}return[0,yE,function(a){return[0,c,b]},g,cf,aS,f,d]}function
gy(b,a){function
c(b,a){return e(o[bw],o[aX],b,a[1])}function
d(a){var
b=a[2],c=a[1];return jN(function(a){return jS(c,a)},b)}return[0,yF,function(c){return[0,b,a]},d,eU,eV,bN,c]}var
gz=[0,yG,f7,jU,eU,eV,bN,function(b,a){return e(o[bw],o[aX],b,a[1])}],jZ=[0,yH,f7,jV,eU,eV,bN,function(b,a){return e(o[bw],o[aX],b,a[1])}];function
yI(b){var
a=eS([0,gz,0],eJ(r[96],r[94],r[97],r[98],b)[1]);if(a){var
d=a[1],e=function(a){return a[1]};return[0,c(g[17],e,d)]}return 0}var
yJ=a(bh(o[eX],r[e6],r[e3],r[e5],r[e9],dW,o[e1]),[0,jX,0]);function
yK(b){var
c=[0,gw(yL,[0,b]),0];return a(bh(o[eX],r[e6],r[e3],r[e5],r[e9],dW,o[e1]),c)}var
yM=dY([0,jY,0]);function
yN(a){return dY([0,gx(yO,[0,a]),0])}function
yP(b){var
c=[0,gy(yQ,[0,b]),0];return a(bh(o[e7],r[96],r[94],r[97],r[98],dV,o[eZ]),c)}var
yS=[0,gy(yR,0),0],yT=a(bh(o[e7],r[96],r[94],r[97],r[98],dV,o[eZ]),yS),yV=[0,gw(yU,0),0],yW=a(bh(o[eX],r[e6],r[e3],r[e5],r[e9],dW,o[e1]),yV),yY=dY([0,gx(yX,0),0]),yZ=a(bh(o[e7],r[96],r[94],r[97],r[98],dV,o[eZ]),[0,gz,0]),y0=a(bh(o[e7],r[96],r[94],r[97],r[98],dV,o[eZ]),[0,jZ,0]),y1=dY([0,gv,0]),Y=[0,sr,ss,eF,f5,eG,f6,f7,eH,bq,aM,a3,f9,eI,cS,sM,eJ,ao,sO,iT,o,dP,xc,xd,jA,xf,dQ,dR,dS,dT,dU,gj,gk,bu,eR,jC,aE,bN,gl,gm,dV,dW,xC,jD,jE,jF,jG,gn,xD,jH,jI,eS,eS,xK,jJ,jK,bg,eT,go,gp,gq,bh,jM,dY,jN,jO,jP,gr,jQ,jR,gs,dZ,gt,jS,ce,cf,aS,eU,eV,cg,gu,jT,jU,jV,jW,jX,jY,gv,gw,gx,gy,gz,jZ,yI,yJ,yK,yM,yN,yP,yT,yW,yY,yZ,y0,y1,a(bh(o[eX],r[e6],r[e3],r[e5],r[e9],dW,o[e1]),[0,gv,0])];aU(729,Y,"Micromega_plugin.Coq_micromega");a(y2[10],ae);var
y3=0,y5=[0,[0,y4,function(a){return al[55]}],y3];x(w[10][8],ae,y6,0,y5);var
y7=0;function
y8(b,a){var
d=c(w[13][24],a,b);return c(Y[98],-1,d)}var
y_=a(F[1][7],y9),y$=[0,[5,a(T[16],w[2][1])],y_],zb=[0,[0,[0,za,[1,c(U[11],0,y$),0]],y8],y7];function
zc(d,b,a){var
e=c(w[13][24],a,b);return c(Y[98],d,e)}var
ze=a(F[1][7],zd),zf=[0,[5,a(T[16],w[2][1])],ze],zg=[1,c(U[11],0,zf),0],zi=a(F[1][7],zh),zj=[0,[5,a(T[16],gA[6])],zi],zl=[0,[0,[0,zk,[1,c(U[11],0,zj),zg]],zc],zb];x(w[10][8],ae,zm,0,zl);var
zn=0;function
zo(d,b){var
e=c(w[13][24],b,d);return a(Y[ku],e)}var
zq=a(F[1][7],zp),zr=[0,[5,a(T[16],w[2][1])],zq],zt=[0,[0,[0,zs,[1,c(U[11],0,zr),0]],zo],zn];x(w[10][8],ae,zu,0,zt);var
zv=0;function
zw(d,b){var
e=c(w[13][24],b,d);return a(Y[103],e)}var
zy=a(F[1][7],zx),zz=[0,[5,a(T[16],w[2][1])],zy],zB=[0,[0,[0,zA,[1,c(U[11],0,zz),0]],zw],zv];x(w[10][8],ae,zC,0,zB);var
zD=0;function
zE(d,b){var
e=c(w[13][24],b,d);return a(Y[104],e)}var
zG=a(F[1][7],zF),zH=[0,[5,a(T[16],w[2][1])],zG],zJ=[0,[0,[0,zI,[1,c(U[11],0,zH),0]],zE],zD];x(w[10][8],ae,zK,0,zJ);var
zL=0;function
zM(d,b){var
e=c(w[13][24],b,d);return a(Y[105],e)}var
zO=a(F[1][7],zN),zP=[0,[5,a(T[16],w[2][1])],zO],zR=[0,[0,[0,zQ,[1,c(U[11],0,zP),0]],zM],zL];x(w[10][8],ae,zS,0,zR);var
zT=0;function
zU(d,b){var
e=c(w[13][24],b,d);return a(Y[99],e)}var
zW=a(F[1][7],zV),zX=[0,[5,a(T[16],w[2][1])],zW],zZ=[0,[0,[0,zY,[1,c(U[11],0,zX),0]],zU],zT];x(w[10][8],ae,z0,0,zZ);var
z1=0;function
z2(d,b){var
e=c(w[13][24],b,d);return a(Y[kr],e)}var
z4=a(F[1][7],z3),z5=[0,[5,a(T[16],w[2][1])],z4],z7=[0,[0,[0,z6,[1,c(U[11],0,z5),0]],z2],z1];x(w[10][8],ae,z8,0,z7);var
z9=0;function
z_(d,b){var
e=c(w[13][24],b,d);return a(Y[101],e)}var
Aa=a(F[1][7],z$),Ab=[0,[5,a(T[16],w[2][1])],Aa],Ad=[0,[0,[0,Ac,[1,c(U[11],0,Ab),0]],z_],z9];x(w[10][8],ae,Ae,0,Ad);var
Af=0;function
Ag(d,b){var
e=c(w[13][24],b,d);return a(Y[94],e)}var
Ai=a(F[1][7],Ah),Aj=[0,[5,a(T[16],w[2][1])],Ai],Al=[0,[0,[0,Ak,[1,c(U[11],0,Aj),0]],Ag],Af];x(w[10][8],ae,Am,0,Al);var
An=0;function
Ao(d,b){var
e=c(w[13][24],b,d);return a(Y[96],e)}var
Aq=a(F[1][7],Ap),Ar=[0,[5,a(T[16],w[2][1])],Aq],At=[0,[0,[0,As,[1,c(U[11],0,Ar),0]],Ao],An];x(w[10][8],ae,Au,0,At);var
Av=0;function
Aw(b,a){var
d=c(w[13][24],a,b);return c(Y[97],-1,d)}var
Ay=a(F[1][7],Ax),Az=[0,[5,a(T[16],w[2][1])],Ay],AB=[0,[0,[0,AA,[1,c(U[11],0,Az),0]],Aw],Av];function
AC(d,b,a){var
e=c(w[13][24],a,b);return c(Y[97],d,e)}var
AE=a(F[1][7],AD),AF=[0,[5,a(T[16],w[2][1])],AE],AG=[1,c(U[11],0,AF),0],AI=a(F[1][7],AH),AJ=[0,[5,a(T[16],gA[6])],AI],AL=[0,[0,[0,AK,[1,c(U[11],0,AJ),AG]],AC],AB];x(w[10][8],ae,AM,0,AL);var
AN=0;function
AO(b,a){var
d=c(w[13][24],a,b);return c(Y[95],-1,d)}var
AQ=a(F[1][7],AP),AR=[0,[5,a(T[16],w[2][1])],AQ],AT=[0,[0,[0,AS,[1,c(U[11],0,AR),0]],AO],AN];function
AU(d,b,a){var
e=c(w[13][24],a,b);return c(Y[95],d,e)}var
AW=a(F[1][7],AV),AX=[0,[5,a(T[16],w[2][1])],AW],AY=[1,c(U[11],0,AX),0],A0=a(F[1][7],AZ),A1=[0,[5,a(T[16],gA[6])],A0],A3=[0,[0,[0,A2,[1,c(U[11],0,A1),AY]],AU],AT];x(w[10][8],ae,A4,0,A3);var
j0=[0,ae];aU(735,j0,"Micromega_plugin.G_micromega");aU(736,[0,gP,k,r,p,B,aB,cR,Y,j0],"Micromega_plugin");return}
