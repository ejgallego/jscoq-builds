function(uN){"use strict";var
h5="QMicromega",eq=115,fG="i",iu="__varmap",ic="monoid",it="__p",h4="__p%i",is="__ff",X="ZMicromega",fA=" * ",I="micromega",ib="buggy certificate",es=123,et='command "',ir="( )^2",cB="plugins/micromega/certificate.ml",hU="compare_num",fF="real_nonlinear_prover",h2=" [*] ",h3="__wit",h1="Zero",fE="Lia",iq="scale_term : not implemented",ia=142,br="RingMicromega",h0='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',ip="parse_zop",fy="real nonlinear prover",io="0",fD="%a * %a",hT="__arith",cA="Reals",er=112,ep=438,bs=248,hR="<=",hS="QArith",im=146,fC=" + ",y="Coq",h$="psatz_R",h9="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",h_="Bad logical fragment",d=246,fx="plugins/micromega/mfourier.ml",N="Tauto",il="%a + %a",hZ=" Cannot find witness",ik="the use of a specialized external tool called csdp. \n\n",hY=120,h8="EnvRing",W="t",hX="PsatzZ",ij="Rdefinitions",eo=113,hW="D",h6="linear prover",h7="Depth",ii="Timeout",hV=" Skipping what remains of this tactic: the complexity of the goal requires ",ig="psatz_Q",ih='" exited ',fB=117,id="psatz_Z",ie="Rpow_def",hQ="%i",en=114,fz="pure_sos",ad="VarMap",i=250,D=uN.jsoo_runtime,B=D.caml_check_bound,aj=D.caml_equal,bq=D.caml_fresh_oo_id,ab=D.caml_int_compare,hP=D.caml_int_of_string,fv=D.caml_lessthan,uL=D.caml_list_of_js_array,hO=D.caml_ml_string_length,el=D.caml_mul,c=D.caml_new_string,h=D.caml_obj_tag,aJ=D.caml_register_global,w=D.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):D.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):D.caml_call_gen(a,[b,c])}function
f(a,b,c,d){return a.length==3?a(b,c,d):D.caml_call_gen(a,[b,c,d])}function
aa(a,b,c,d,e){return a.length==4?a(b,c,d,e):D.caml_call_gen(a,[b,c,d,e])}function
V(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):D.caml_call_gen(a,[b,c,d,e,f])}function
M(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):D.caml_call_gen(a,[b,c,d,e,f,g])}function
fw(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):D.caml_call_gen(a,[b,c,d,e,f,g,h])}function
em(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):D.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
uM(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):D.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
t=D.caml_get_global_data(),cG=[0,0,0],eW=[0,0],c8=[0,0,0],hg=uL([[0,c(y),[0,c("Lists"),[0,c("List"),0]]],[0,c(X),0],[0,c(N),0],[0,c(br),0],[0,c(h8),0],[0,c(y),[0,c(I),[0,c(X),0]]],[0,c(y),[0,c(I),[0,c("RMicromega"),0]]],[0,c(y),[0,c(I),[0,c(N),0]]],[0,c(y),[0,c(I),[0,c(br),0]]],[0,c(y),[0,c(I),[0,c(h8),0]]],[0,c(y),[0,c(hS),[0,c("QArith_base"),0]]],[0,c(y),[0,c(cA),[0,c(ij),0]]],[0,c(y),[0,c(cA),[0,c(ie),0]]],[0,c("LRing_normalise"),0]]),ai=c("micromega_plugin"),l=t.Stdlib,e=t.Num,r=t.Stdlib__printf,o=t.Big_int,C=t.Unix,cT=t.Stdlib__marshal,eT=t.Stdlib__printexc,q=t.Stdlib__list,ay=t.Assert_failure,gn=t.Ratio,gr=t.Stdlib__set,J=t.Not_found,bA=t.Stdlib__hashtbl,cU=t.Stdlib__map,m=t.Util,a7=t.CErrors,al=t.Option,g=t.CamlinternalLazy,a9=t.Names,j=t.EConstr,hE=t.CAst,ah=t.Tactics,aI=t.Tacmach,F=t.Tacticals,bn=t.Pp,cw=t.Proofview,ci=t.Coqlib,fi=t.Goptions,v=t.Ltac_plugin,R=t.Genarg,fu=t.Stdarg,iv=c(io),iw=[0,[12,118,[2,0,0]],c("v%s")],ix=[0,[11,c("1/("),[15,[12,41,0]]],c("1/(%a)")],iy=[0,[11,c("- ("),[15,[12,41,0]]],c("- (%a)")],iz=[0,[12,40,[15,[11,c(")+("),[15,[12,41,0]]]]],c("(%a)+(%a)")],iA=[0,[12,40,[15,[11,c(")-("),[15,[12,41,0]]]]],c("(%a)-(%a)")],iB=[0,[12,40,[15,[11,c(")*("),[15,[12,41,0]]]]],c("(%a)*(%a)")],iC=[0,[12,40,[15,[11,c(")/("),[15,[12,41,0]]]]],c("(%a)/(%a)")],iD=[0,[12,40,[15,[11,c(")^("),[4,3,0,0,[12,41,0]]]]],c("(%a)^(%i)")],iE=[0,[11,c("Aeq("),[4,3,0,0,[12,41,0]]],c("Aeq(%i)")],iF=[0,[11,c("Ale("),[4,3,0,0,[12,41,0]]],c("Ale(%i)")],iG=[0,[11,c("Alt("),[4,3,0,0,[12,41,0]]],c("Alt(%i)")],iH=[0,[11,c("eq("),[2,0,[12,41,0]]],c("eq(%s)")],iI=[0,[11,c("le("),[2,0,[12,41,0]]],c("le(%s)")],iJ=[0,[11,c("lt("),[2,0,[12,41,0]]],c("lt(%s)")],iK=[0,[12,40,[15,[11,c(")^2"),0]]],c("(%a)^2")],iL=[0,[11,c(ic),0],c(ic)],iM=[0,[15,[11,c(fA),[15,0]]],c(fD)],iN=[0,[15,[11,c(fC),[15,0]]],c(il)],iO=[0,[15,[11,c(fA),[15,0]]],c(fD)],kb=[0,0],km=[0,0,0],kn=[0,[0,0],0],j7=[0,0],j6=[0,0],j5=[0,0],jX=[0,[0,0]],jY=[0,[0,0]],jZ=[0,[0,0]],j0=[0,[0,0]],jT=[0,[0,0]],jU=[0,[0,0]],jV=[0,[0,0]],jW=[0,[0,0]],jz=[0,[0,0],0],jx=[0,0,0],jq=[0,1],jr=[0,2],js=[0,3],jl=[0,0],jn=[0,0],jm=[0,1],jp=[0,3],jo=[0,0],i3=[0,[1,0]],i4=[0,0,[0,0]],i5=[0,[0,0],0],i6=[0,0],i7=[0,[1,0]],i8=[0,[1,0]],i9=[0,0],i_=[0,[1,0]],i$=[0,[1,0]],ja=[0,[1,0]],jb=[0,0],jc=[0,[1,0]],jd=[0,0,0],je=[0,0,0],jf=[0,0],jg=[0,0,0],jh=[0,0],iZ=[1,0],iY=[0,0],iQ=[1,0],iR=[1,0],iS=[0,0],iU=[0,0],iT=[0,0],jE=[0,0],jR=[0,0],j$=[0,0],kd=[0,[0,0],0],ke=[0,0,0],kf=[0,[0,0],0],kg=[0,0,0],kh=[0,[0,0],0],ki=[0,0,0],kj=[0,0,0],kk=[0,0,0],ko=[0,[0,0],0],kp=[0,0,0],kq=[0,[0,0],0],kr=[0,0,0],ks=[0,[0,0],0],kt=[0,0,0],ku=[0,0,0],kv=[0,0,0],kx=c(";"),kV=[0,[11,c(et),[2,0,[11,c(ih),[4,3,0,0,0]]]],c('command "%s" exited %i')],kU=[0,[11,c(et),[2,0,[11,c(ih),[2,0,0]]]],c('command "%s" exited %s')],kW=[0,[11,c(et),[2,0,[11,c('" killed '),[4,3,0,0,0]]]],c('command "%s" killed %i')],kX=[0,[11,c(et),[2,0,[11,c('" stopped '),[4,3,0,0,0]]]],c('command "%s" stopped %i')],kK=[0,c("plugins/micromega/mutils.ml"),191,7],kE=[0,0,0],lo=[0,[11,c(h1),0],c(h1)],lp=[0,[11,c("Hyp "),[4,3,0,0,0]],c("Hyp %i")],lq=[0,[11,c("Def "),[4,3,0,0,0]],c("Def %i")],lr=[0,[11,c("Cst "),[2,0,0]],c("Cst %s")],ls=[0,[11,c(ir),0],c(ir)],lt=[0,[11,c("P * "),[15,0]],c("P * %a")],lu=[0,[12,40,[15,[11,c(")/"),[2,0,0]]]],c("(%a)/%s")],lv=[0,[15,[11,c(fA),[15,0]]],c(fD)],lw=[0,[15,[11,c(fC),[15,0]]],c(il)],lx=[0,[12,91,[15,[12,93,0]]],c("[%a]")],ly=[0,[12,46,0],c(".")],lz=[0,[4,3,0,0,[11,c(":= "),[15,[11,c(" ; "),[15,0]]]]],c("%i:= %a ; %a")],lA=[0,[4,3,0,0,[12,es,[15,[11,c(hR),[15,[11,c(hR),[15,[12,125,[15,0]]]]]]]]],c("%i{%a<=%a<=%a}%a")],lJ=[0,0],lI=[0,0],lH=[0,0],lG=[0,0,[0,0]],lF=[0,0,[0,0]],lC=[0,c("plugins/micromega/polynomial.ml"),509,10],lm=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c("%a %s %s")],lk=c(">="),lj=c("="),ld=c(hU),le=c(hU),lf=[0,0],lc=[0,0],la=[0,0],k_=[0,[2,0,[12,hY,[4,3,0,0,[11,c(fC),0]]]],c("%sx%i + ")],k3=[0,0],k2=[0,1],k0=[0,0],lN=[0,[12,72,[4,3,0,0,0]],c("H%i")],lO=[0,[11,c("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("E(%i,%a,%a)")],lP=[0,[11,c("A("),[15,[12,44,[15,[12,41,0]]]]],c("A(%a,%a)")],mB=[0,1],mC=[0,[0,0,0]],my=c("merge_proof : pivot is not possible"),mz=[0,1],mA=[0,1],mx=[0,0],mr=[0,1],ms=[0,1],mt=[0,1],mu=[0,1],mv=[0,1],mw=[0,1],mn=[0,0],mo=[0,-1],mp=[0,[11,c("optimise Exception : "),[2,0,0]],c("optimise Exception : %s")],ml=[0,0,0],mc=[0,0],md=[0,0],me=[0,0],mf=[0,0],mg=[0,0],mh=[0,0],mi=[0,0],mj=[0,0],mb=[0,0],ma=c("bound_of_variable: impossible"),l$=[0,0,0],l9=[0,0,0],l_=c("SystemContradiction"),l8=[0,0],l5=[0,0],l4=[0,0,0,0],l1=[0,0],lZ=[0,0],l0=[0,0],l2=[0,c(fx),257,4],lX=[0,c(fx),209,9],lU=[0,1],lV=[0,0],lR=[0,c(fx),147,4],lK=[0,1],lM=c("Mfourier.SystemContradiction"),lT=c("Mfourier.TimeOut"),m3=c(iq),m4=c("scale term: not implemented"),m5=c(iq),m6=[0,0],m7=c("term_to_q_expr: not implemented"),na=[0,0],nb=c("term_to_z_expr: not implemented"),nh=c("Cuts should already be compiled"),nI=[0,0],nJ=[0,1],nK=[0,0],nL=[0,1],nM=[0,c(cB),1098,1],nD=[0,c(cB),997,2],nE=[0,0],nF=[0,1],nG=[0,c(cB),1024,2],nA=[0,1],nB=[0,0,0],nC=c("Interval without proof"),ny=[0,0],nv=[0,1],nw=[0,-1],ns=[0,1],nt=[0,-1],nl=[0,1],nm=[0,c(cB),679,5],nn=[0,0],nj=c("proof_of_farkas : not enough hyps"),ng=[0,[0,0]],nf=c("id_of_hyp"),nd=[0,0],ne=[0,1],m$=[0,0],m8=[0,1],m9=[0,0],m2=[0,c(cB),404,12],mZ=c("cannot happen"),mS=c("make_certificate(1)"),mT=c("empty_certificate"),mQ=[0,1],mP=[0,0],mH=[0,0],mM=[0,[0,0],0],mN=[0,0,0],mU=c("Certificate.Strict"),no=c("Certificate.FoundProof"),nV=[0,0,0],nT=[0,0,0],nR=[0,0,[0,5,0]],nU=[0,1,[0,4,[0,5,0]]],nS=[0,1,[0,6,[0,5,0]]],nP=[0,1,[0,6,[0,5,0]]],nN=c("Persistent_cache.PHashtable(Key).InvalidTableFormat"),nO=c("Persistent_cache.PHashtable(Key).UnboundTable"),rH=[0,[12,68,0],c(hW)],rI=[0,[11,c("R["),[15,[12,44,[15,[12,93,0]]]]],c("R[%a,%a]")],rJ=[0,[11,c("C["),[15,[12,44,[15,[12,93,0]]]]],c("C[%a,%a]")],rK=c("]"),rL=c("["),rM=[0,[11,c("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],c("EP[%a,%a,%a]")],rV=c("abstract_wrt_formula"),s6=c(fF),s4=c(fF),s1=c(fF),sU=c(fy),sT=c(fy),sS=c(fy),sx=c(ib),sw=c(ib),sp=c("csdpcert"),sq=c(I),sr=c("plugins"),sh=c(hT),si=[0,0],sj=c(hZ),sb=c(h_),sc=c(ii),sd=c(h9),se=c(h0),sf=c(ik),sg=c(hV),r7=c(h3),r8=c(W),r9=[0,[0,c(y),[0,c(I),[0,c(ad),0]]],[0,[0,c(ad),0],0]],r_=c(ad),r$=c(iu),sa=c(is),r3=c(hT),r4=[0,0],r5=c(hZ),rX=c(h_),rY=c(ii),rZ=c(h9),r0=c(h0),r1=c(ik),r2=c(hV),rT=c("bad old index"),rU=c("proof compaction error"),rR=[0,0],rO=c(h3),rP=c(iu),rQ=c(is),rG=[0,[15,[12,47,[15,0]]],c("%a/%a")],rD=c(W),rE=[0,[0,c(y),[0,c(I),[0,c(ad),0]]],[0,[0,c(ad),0],0]],rF=c(ad),rA=c("Empty"),rB=[0,[0,c(y),[0,c(I),[0,c(ad),0]]],[0,[0,c(ad),0],0]],rC=c(ad),rx=c("Leaf"),ry=[0,[0,c(y),[0,c(I),[0,c(ad),0]]],[0,[0,c(ad),0],0]],rz=c(ad),ru=c("Node"),rv=[0,[0,c(y),[0,c(I),[0,c(ad),0]]],[0,[0,c(ad),0],0]],rw=c(ad),rc=[0,0,0],rt=[0,[11,c(it),[4,3,0,0,0]],c(h4)],rs=[0,[11,c(it),[4,3,0,0,0]],c(h4)],rr=[0,[11,c("__x"),[4,3,0,0,0]],c("__x%i")],rq=[0,c("plugins/micromega/coq_micromega.ml"),1226,11],ri=c("error : parse_arith(2)"),rg=c("parse_qexpr parse error"),re=[0,0],qV=c("get_rank"),qU=[1,c("Oups")],qS=c(ip),qQ=c(ip),qH=[0,[12,48,0],c(io)],qI=[0,[11,c("(In "),[15,[12,41,[12,37,[11,c("nat"),0]]]]],c("(In %a)%%nat")],qJ=[0,[12,40,[15,[11,c("^2)"),0]]],c("(%a^2)")],qK=[0,[11,c("( "),[15,[11,c(h2),[15,[12,41,0]]]]],c("( %a [*] %a)")],qL=[0,[12,40,[15,[11,c(h2),[15,[12,41,0]]]]],c("(%a [*] %a)")],qM=[0,[12,40,[15,[11,c(" [+] "),[15,[12,41,0]]]]],c("(%a [+] %a)")],qN=[0,[12,40,[15,[12,41,[12,37,[11,c("positive"),0]]]]],c("(%a)%%positive")],qE=[0,[11,c("Pc "),[15,0]],c("Pc %a")],qF=[0,[11,c("Pinj("),[15,[12,44,[15,[12,41,0]]]]],c("Pinj(%a,%a)")],qG=[0,[11,c("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("PX(%a,%a,%a)")],qB=[0,[15,[11,c(" ,"),[15,0]]],c("%a ,%a")],qC=[0,[15,0],c("%a")],qD=[0,[2,0,[15,[2,0,0]]],c("%s%a%s")],qA=[0,[2,0,0],c("%s")],qz=[0,[4,3,0,0,0],c(hQ)],qy=[0,[4,3,0,0,0],c(hQ)],qt=c("Formula"),qu=[0,[0,c(y),[0,c(I),[0,c(br),0]]],[0,[0,c(br),0],0]],qv=c(br),qq=c("Build_Formula"),qr=[0,[0,c(y),[0,c(I),[0,c(br),0]]],[0,[0,c(br),0],0]],qs=c(br),qn=c("QWitness"),qo=[0,[0,c(y),[0,c(I),[0,c(h5),0]]],0],qp=c(h5),qk=c("BFormula"),ql=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],qm=c(X),qh=c("I"),qi=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],qj=c(X),qe=c("X"),qf=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],qg=c(X),qb=c("A"),qc=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],qd=c(X),p_=c("N"),p$=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],qa=c(X),p7=c(hW),p8=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],p9=c(X),p4=c("Cj"),p5=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],p6=c(X),p1=c("FF"),p2=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],p3=c(X),pY=c("TT"),pZ=[0,[0,c(y),[0,c(I),[0,c(N),0]]],[0,[0,c(N),0],0]],p0=c(X),pX=c(hX),pW=c("PsatzC"),pV=c("PsatzAdd"),pU=c("PsatzMulC"),pT=c("PsatzMulE"),pS=c("PsatzSquare"),pR=c("PsatzIn"),pQ=c("OpGt"),pP=c("OpGe"),pO=c("OpLt"),pN=c("OpLe"),pM=c("OpNEq"),pL=c("OpEq"),pK=c("Pinj"),pJ=c("Pc"),pI=c("PX"),pH=c("PEpow"),pG=c("PEsub"),pF=c("PEmul"),pE=c("PEopp"),pD=c("PEadd"),pC=c("PEc"),pB=c("PEX"),pA=c("Q2R"),pz=c("IZR"),py=c("pow"),px=c("Rinv"),pw=c("Rmult"),pv=c("Ropp"),pu=c("Rminus"),pt=c("Rplus"),pr=c("Rlt"),pp=c("Rle"),pn=c("Rge"),pl=c("Rgt"),pk=c("Qpower"),pj=c("Qmult"),pi=c("Qopp"),ph=c("Qminus"),pg=c("Qplus"),pe=c("Qeq"),pc=c("Qlt"),pa=c("Qle"),o$=c("Z.pow"),o_=c("Z.mul"),o9=c("Z.opp"),o8=c("Z.sub"),o7=c("Z.add"),o6=c("eq"),o4=c("Z.lt"),o2=c("Z.le"),o0=c("Z.ge"),oY=c("Z.gt"),oX=c("EnumProof"),oW=c("CutProof"),oV=c("RatProof"),oU=c("DoneProof"),oT=c("ZArithProof"),oS=c("R1"),oR=c("R0"),oQ=c("COpp"),oP=c("CInv"),oO=c("CMult"),oN=c("CMinus"),oM=c("CPlus"),oL=c("CZ"),oK=c("CQ"),oJ=c("C1"),oI=c("C0"),oH=c("Rcst"),oG=c("Qmake"),oF=c("R"),oE=c("Q"),oD=c("Zneg"),oC=c("Zpos"),oB=c("Z0"),oA=c("Z"),oz=c("xI"),oy=c("xO"),ox=c("xH"),ow=c("Npos"),ov=c("N0"),ou=c("S"),ot=c("O"),os=c("list"),or=c("nil"),oq=c("cons"),op=c("False"),oo=c("True"),on=c("iff"),om=c("not"),ol=c("or"),ok=c("and"),n5=[0,0,0],nW=c(""),nY=[0,c(fE),[0,c("Enum"),0]],nZ=c("Lia Enum"),n1=[0,c("Lra"),[0,c(h7),0]],n3=[0,c(fE),[0,c(h7),0]],n6=[0,c(y),[0,c("Logic"),[0,c("Decidable"),0]]],n$=[0,[0,c(y),[0,c("Numbers"),[0,c("BinNums"),0]]],0],oa=[0,[0,c(y),[0,c(cA),[0,c(ij),0]]],[0,[0,c(y),[0,c(cA),[0,c(ie),0]]],[0,[0,c(y),[0,c(cA),[0,c("Raxioms"),0]]],[0,[0,c(y),[0,c(hS),[0,c("Qreals"),0]]],0]]]],ob=[0,[0,c(y),[0,c("ZArith"),[0,c("BinInt"),0]]],0],oe=c(X),of=c(X),og=c(X),oh=c(X),oi=c(X),oj=c(X),qw=c("Coq_micromega.M.ParseError"),rW=c("Coq_micromega.CsdpNotFound"),sl=c(".csdp.cache"),sm=c("csdp"),sA=c(".lia.cache"),sD=c(".nia.cache"),sG=c(".nra.cache"),sK=c(h6),sO=c(h6),sR=c("nra"),sV=c("lia"),sX=c("nlia"),s7=c(fz),s_=c(fz),tb=c(fz),ti=[0,c("myred"),0],tk=c("RED"),tn=c(W),to=c(id),tr=c(W),tt=c(fG),tu=c(id),tw=c(hX),tz=c(W),tA=c("xlia"),tC=c(fE),tF=c(W),tG=c("xnlia"),tI=c("Nia"),tL=c(W),tM=c("xnra"),tO=c("NRA"),tR=c(W),tS=c("xnqa"),tU=c("NQA"),tX=c(W),tY=c("sos_Z"),t0=c("Sos_Z"),t3=c(W),t4=c("sos_Q"),t6=c("Sos_Q"),t9=c(W),t_=c("sos_R"),ua=c("Sos_R"),ud=c(W),ue=c("lra_Q"),ug=c("LRA_Q"),uj=c(W),uk=c("lra_R"),um=c("LRA_R"),up=c(W),uq=c(h$),ut=c(W),uv=c(fG),uw=c(h$),uy=c("PsatzR"),uB=c(W),uC=c(ig),uF=c(W),uH=c(fG),uI=c(ig),uK=c("PsatzQ"),l7=t.CMap,mY=t.CList,nQ=t.End_of_file,so=t.Coq_config,ss=t.Envars,st=t.Stdlib__filename,rS=t.Failure,ro=t.Retyping,rp=t.Sorts,qX=t.Pfedit,qW=t.Invalid_argument,qO=t.Reductionops,oc=t.UnivGen,sn=t.System,tg=t.Mltop;function
ae(d,c){if(typeof
c==="number")return b(l[55],d,iv);else
switch(c[0]){case
0:var
m=a(e[40],c[1]);return b(l[55],d,m);case
1:return f(r[1],d,iw,c[1]);case
2:return aa(r[1],d,ix,ae,c[1]);case
3:return aa(r[1],d,iy,ae,c[1]);case
4:var
g=c[1];return M(r[1],d,iz,ae,g[1],ae,g[2]);case
5:var
h=c[1];return M(r[1],d,iA,ae,h[1],ae,h[2]);case
6:var
i=c[1];return M(r[1],d,iB,ae,i[1],ae,i[2]);case
7:var
j=c[1];return M(r[1],d,iC,ae,j[1],ae,j[2]);default:var
k=c[1];return V(r[1],d,iD,ae,k[1],k[2])}}function
bF(d,c){switch(c[0]){case
0:return f(r[1],d,iE,c[1]);case
1:return f(r[1],d,iF,c[1]);case
2:return f(r[1],d,iG,c[1]);case
3:var
g=a(e[40],c[1]);return f(r[1],d,iH,g);case
4:var
h=a(e[40],c[1]);return f(r[1],d,iI,h);case
5:var
i=a(e[40],c[1]);return f(r[1],d,iJ,i);case
6:return aa(r[1],d,iK,ae,c[1]);case
7:return b(r[1],d,iL);case
8:return M(r[1],d,iM,ae,c[1],bF,c[2]);case
9:return M(r[1],d,iN,bF,c[1],bF,c[2]);default:return M(r[1],d,iO,bF,c[1],bF,c[2])}}var
fH=[0,ae,bF];aJ(596,fH,"Micromega_plugin.Sos_types");function
bt(a){return 0===a?1:0}function
bu(a,b){if(a){var
c=a[1];return[0,c,bu(a[2],b)]}return b}function
fI(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
eu(b,a){return b?[0,eu(b[1],a)]:a}var
iP=[0];function
aK(a){return typeof
a==="number"?iQ:0===a[0]?[1,aK(a[1])]:[0,a[1]]}function
bG(b,a){if(typeof
b==="number")return typeof
a==="number"?iR:0===a[0]?[1,aK(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,aK(c)]:0===a[0]?[1,bS(c,a[1])]:[0,bG(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,bG(d,a[1])]:[1,bG(d,a[1])]}}function
bS(b,a){if(typeof
b==="number")return typeof
a==="number"?iS:0===a[0]?[0,aK(a[1])]:[1,aK(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,aK(c)]:0===a[0]?[0,bS(c,a[1])]:[1,bS(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,aK(d)]:0===a[0]?[1,bS(d,a[1])]:[0,bG(d,a[1])]}}function
bT(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,bT(a[1])]}function
bU(a){return typeof
a==="number"?0===a?iT:1:[0,[0,a[1]]]}function
bV(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
fJ(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,bT(a[1])]]}function
bH(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?bV(bH(c,a[1])):bU(bH(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,bT(d)]:0===a[0]?bU(bW(d,a[1])):bV(bH(d,a[1]))}}function
bW(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,bT(c)]:0===a[0]?bU(bW(c,a[1])):bV(bH(c,a[1]))}var
d=b[1];return typeof
a==="number"?fJ(d):0===a[0]?bV(bW(d,a[1])):bU(bW(d,a[1]))}}function
ev(c,b){var
a=bH(c,b);return typeof
a==="number"?0:a[1]}function
ew(b,a){return typeof
b==="number"?a:0===b[0]?bG(a,[1,ew(b[1],a)]):[1,ew(b[1],a)]}function
bX(a){return typeof
a==="number"?iU:0===a[0]?[0,bX(a[1])]:[0,bX(a[1])]}function
fK(h,g,f){var
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
iV=0;function
fL(a,b){return fK(iV,a,b)}function
ex(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(fL(e,f)){case
0:return b;case
1:var
c=d,a=b,b=ev(f,e);continue;default:var
c=d,b=ev(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,ex(d,g,a[1])]}}}return 0}}function
iW(b,a){var
c=bX(a);return ex(eu(bX(b),c),b,a)}function
fM(a){return a?aK(fM(a[1])):0}var
x=[0,aK,bG,bS,bT,bU,bV,fJ,bH,bW,ev,ew,bX,fK,fL,ex,iW,fM],iX=[0,function(b){return b?[0,a(x[17],b[1])]:0}];function
cC(a,d,c){if(typeof
c==="number")return d;else{if(0===c[0]){var
e=cC(a,d,c[1]);return b(a,d,b(a,e,e))}var
f=cC(a,d,c[1]);return b(a,f,f)}}function
fN(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
bI(c,b){if(b){var
d=b[1],e=bI(c,b[2]);return[0,a(c,d),e]}return 0}function
ey(d,c,a){if(a){var
e=a[1];return b(d,e,ey(d,c,a[2]))}return c}function
ez(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
fO(b){return typeof
b==="number"?iY:0===b[0]?[0,[0,b[1]]]:[1,a(x[4],b[1])]}function
fP(b){return typeof
b==="number"?iZ:0===b[0]?[0,a(x[4],b[1])]:[1,[0,b[1]]]}function
bv(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?[1,[1,b[1]]]:[1,a(x[4],b[1])];else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?[0,[1,d]]:0===b[0]?ez(bv(d,b[1])):fO(bv(d,b[1]))}var
e=c[1];return typeof
b==="number"?[0,a(x[4],e)]:0===b[0]?fP(bv(e,b[1])):ez(bv(e,b[1]))}}function
a4(c,a){if(typeof
c==="number")return a;else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?c:0===a[0]?[0,b(x[2],d,a[1])]:bv(d,a[1])}var
e=c[1];return typeof
a==="number"?c:0===a[0]?bv(a[1],e):[1,b(x[2],e,a[1])]}}function
bw(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
cD(b,a){return a4(b,bw(a))}function
bx(c,a){if(typeof
c==="number")return 0;else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?0:0===a[0]?[0,b(x[11],d,a[1])]:[1,b(x[11],d,a[1])]}var
e=c[1];return typeof
a==="number"?0:0===a[0]?[1,b(x[11],e,a[1])]:[0,b(x[11],e,a[1])]}}function
bY(c,a){if(typeof
c==="number")return typeof
a==="number"?0:0===a[0]?1:2;else{if(0===c[0]){var
d=c[1];if(typeof
a!=="number"&&0===a[0])return b(x[14],d,a[1]);return 2}var
e=c[1];if(typeof
a!=="number"&&1===a[0])return fI(b(x[14],e,a[1]));return 1}}function
fQ(b,a){return 2<=bY(b,a)?0:1}function
eA(b,a){return 1===bY(b,a)?1:0}function
i0(b,a){return 2<=bY(b,a)?1:0}function
i1(b,a){return 1===bY(b,a)?a:b}function
cE(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
i2(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
by(b,a){if(typeof
b==="number")return fQ(i3,a)?i4:i5;else{if(0===b[0]){var
e=by(b[1],a),f=e[1],c=a4(bx(i7,e[2]),i6);if(eA(c,a))return[0,bx(i8,f),c];var
i=cD(c,a);return[0,a4(bx(i_,f),i9),i]}var
g=by(b[1],a),h=g[1],d=bx(i$,g[2]);if(eA(d,a))return[0,bx(ja,h),d];var
j=cD(d,a);return[0,a4(bx(jc,h),jb),j]}}function
fR(b,a){if(typeof
b==="number")return jd;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return je;else{if(0===a[0])return by(c,a);var
d=by(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,bw(f),0];var
l=a4(a,e);return[0,bw(a4(f,jf)),l]}}var
g=b[1];if(typeof
a==="number")return jg;else{if(0===a[0]){var
h=by(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,bw(j),0];var
m=cD(a,i);return[0,bw(a4(j,jh)),m]}var
k=by(g,[0,a[1]]),n=k[1];return[0,n,bw(k[2])]}}}function
ji(b,a){return fR(b,a)[1]}var
s=[0,ez,fO,fP,bv,a4,bw,cD,bx,bY,fQ,eA,i0,i1,cE,i2,by,fR,ji,function(c,a){if(typeof
c==="number")return cE(a);else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?cE(c):0===a[0]?[0,b(x[16],d,a[1])]:[0,b(x[16],d,a[1])]}var
e=c[1];return typeof
a==="number"?cE(c):0===a[0]?[0,b(x[16],e,a[1])]:[0,b(x[16],e,a[1])]}}];function
at(c,a){return 0===b(s[9],c,a)?1:0}function
jj(a){return[0,a]}function
jk(a){return[0,a]}function
eB(d,f,e){var
c=f,a=e;for(;;)switch(c[0]){case
0:var
g=c[1];return 0===a[0]?b(d,g,a[1]):0;case
1:var
h=c[2],i=c[1];if(1===a[0]){var
j=a[2];if(0===b(x[14],i,a[1])){var
c=h,a=j;continue}return 0}return 0;default:var
k=c[3],l=c[2],m=c[1];if(2===a[0]){var
n=a[3],o=a[1];if(0===b(x[14],l,a[2])){if(eB(d,m,o)){var
c=k,a=n;continue}return 0}return 0}return 0}}function
S(c,a){switch(a[0]){case
0:return a;case
1:var
d=a[2];return[1,b(x[2],c,a[1]),d];default:return[1,c,a]}}function
fS(b,c){return typeof
b==="number"?c:0===b[0]?[1,[1,b[1]],c]:[1,a(x[4],b[1]),c]}function
G(f,e,a,d,c){switch(a[0]){case
0:return b(e,a[1],f)?S(0,c):[2,a,d,c];case
1:return[2,a,d,c];default:var
g=a[2],h=a[1];return eB(e,a[3],[0,f])?[2,h,b(x[2],g,d),c]:[2,a,d,c]}}function
fT(c,b,a){return[2,[0,b],a,[0,c]]}function
fU(b,a){return fT(b,a,0)}function
ac(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1];return[1,d,ac(c,b[2])];default:var
e=b[2],f=b[1],g=ac(c,b[3]);return[2,ac(c,f),e,g]}}function
a5(d,a,c){switch(a[0]){case
0:return[0,b(d,a[1],c)];case
1:var
e=a[1];return[1,e,a5(d,a[2],c)];default:var
f=a[2],g=a[1];return[2,g,f,a5(d,a[3],c)]}}function
bJ(d,a,c){switch(a[0]){case
0:return[0,b(d,a[1],c)];case
1:var
e=a[1];return[1,e,bJ(d,a[2],c)];default:var
f=a[2],g=a[1];return[2,g,f,bJ(d,a[3],c)]}}function
bZ(g,f,e,c,d){switch(d[0]){case
0:return S(c,a5(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=b(s[4],m,c);return typeof
h==="number"?S(c,b(f,i,e)):0===h[0]?S(c,b(f,[1,h[1],i],e)):S(m,bZ(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
c==="number"?[2,l,k,b(f,j,e)]:0===c[0]?[2,l,k,bZ(g,f,e,[1,c[1]],j)]:[2,l,k,bZ(g,f,e,a(x[4],c[1]),j)]}}function
b0(h,g,f,e,c,d){switch(d[0]){case
0:var
o=d[1];return S(c,a5(h,ac(g,e),o));case
1:var
j=d[2],n=d[1],i=b(s[4],n,c);return typeof
i==="number"?S(c,b(f,j,e)):0===i[0]?S(c,b(f,[1,i[1],j],e)):S(n,b0(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
c==="number"?[2,m,l,b(f,k,e)]:0===c[0]?[2,m,l,b0(h,g,f,e,[1,c[1]],k)]:[2,m,l,b0(h,g,f,e,a(x[4],c[1]),k)]}}function
eC(f,g,j,d,e,c){switch(c[0]){case
0:return[2,d,e,c];case
1:var
k=c[2],h=c[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,a(x[4],h[1]),k]];default:var
l=c[3],m=c[2],n=c[1],i=b(s[4],m,e);return typeof
i==="number"?G(f,g,b(j,n,d),m,l):0===i[0]?G(f,g,b(j,[2,n,i[1],[0,f]],d),e,l):G(f,g,eC(f,g,j,d,i[1],n),m,l)}}function
eD(g,f,h,k,d,e,c){switch(c[0]){case
0:return[2,ac(f,d),e,c];case
1:var
l=c[2],i=c[1];if(typeof
i==="number")return[2,ac(f,d),e,l];else{if(0===i[0]){var
p=[1,[1,i[1]],l];return[2,ac(f,d),e,p]}var
q=[1,a(x[4],i[1]),l];return[2,ac(f,d),e,q]}default:var
m=c[3],n=c[2],o=c[1],j=b(s[4],n,e);return typeof
j==="number"?G(g,h,b(k,o,d),n,m):0===j[0]?G(g,h,b(k,[2,o,j[1],[0,g]],d),e,m):G(g,h,eD(g,f,h,k,d,j[1],o),n,m)}}function
P(c,e,d,f,g){switch(g[0]){case
0:return a5(e,f,g[1]);case
1:var
q=g[2],r=g[1];return bZ(e,function(a,b){return P(c,e,d,a,b)},q,r,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,a5(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,P(c,e,d,m,h)]:0===k[0]?[2,i,j,P(c,e,d,[1,[1,k[1]],m],h)]:[2,i,j,P(c,e,d,[1,a(x[4],k[1]),m],h)];default:var
n=f[3],o=f[2],p=f[1],l=b(s[4],o,j);if(typeof
l==="number"){var
t=P(c,e,d,n,h);return G(c,d,P(c,e,d,p,i),o,t)}else{if(0===l[0]){var
u=l[1],v=P(c,e,d,n,h);return G(c,d,P(c,e,d,[2,p,u,[0,c]],i),j,v)}var
w=l[1],y=P(c,e,d,n,h);return G(c,d,eC(c,d,function(a,b){return P(c,e,d,a,b)},i,w,p),o,y)}}}}function
z(d,f,g,c,e,h,i){switch(i[0]){case
0:return bJ(g,h,i[1]);case
1:var
t=i[2],u=i[1];return b0(f,c,function(a,b){return z(d,f,g,c,e,a,b)},t,u,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],w=a5(f,ac(c,j),v);return[2,ac(c,k),l,w];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
y=z(d,f,g,c,e,o,j);return[2,ac(c,k),l,y]}else{if(0===m[0]){var
A=z(d,f,g,c,e,[1,[1,m[1]],o],j);return[2,ac(c,k),l,A]}var
B=z(d,f,g,c,e,[1,a(x[4],m[1]),o],j);return[2,ac(c,k),l,B]}default:var
p=h[3],q=h[2],r=h[1],n=b(s[4],q,l);if(typeof
n==="number"){var
C=z(d,f,g,c,e,p,j);return G(d,e,z(d,f,g,c,e,r,k),q,C)}else{if(0===n[0]){var
D=n[1],E=z(d,f,g,c,e,p,j);return G(d,e,z(d,f,g,c,e,[2,r,D,[0,d]],k),l,E)}var
F=n[1],H=z(d,f,g,c,e,p,j);return G(d,e,eD(d,c,e,function(a,b){return z(d,f,g,c,e,a,b)},k,F,r),q,H)}}}}function
b1(f,e,d,a,c){switch(a[0]){case
0:return[0,b(e,a[1],c)];case
1:var
g=a[1];return S(g,b1(f,e,d,a[2],c));default:var
h=a[2],i=a[1],j=b1(f,e,d,a[3],c);return G(f,d,b1(f,e,d,i,c),h,j)}}function
b2(d,g,f,c,e,a){return b(c,a,d)?[0,d]:b(c,a,g)?e:b1(d,f,c,e,a)}function
aL(f,j,i,e,g,d,c,h){switch(h[0]){case
0:return S(c,b2(f,j,i,e,d,h[1]));case
1:var
l=h[2],p=h[1],k=b(s[4],p,c);return typeof
k==="number"?S(c,b(g,l,d)):0===k[0]?S(c,b(g,[1,k[1],l],d)):S(p,aL(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
c==="number"){var
q=b(g,m,d);return G(f,e,aL(f,j,i,e,g,d,0,o),n,q)}else{if(0===c[0]){var
r=aL(f,j,i,e,g,d,[1,c[1]],m);return G(f,e,aL(f,j,i,e,g,d,c,o),n,r)}var
t=aL(f,j,i,e,g,d,a(x[4],c[1]),m);return G(f,e,aL(f,j,i,e,g,d,c,o),n,t)}}}function
Y(b,e,f,d,c,g,h){switch(h[0]){case
0:return b2(b,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return aL(b,e,d,c,function(a,g){return Y(b,e,f,d,c,a,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return b2(b,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?Y(b,e,f,d,c,l,i):0===j[0]?Y(b,e,f,d,c,[1,[1,j[1]],l],i):Y(b,e,f,d,c,[1,a(x[4],j[1]),l],i);return G(b,c,Y(b,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=Y(b,e,f,d,c,n,i),u=0,v=aL(b,e,d,c,function(a,g){return Y(b,e,f,d,c,a,g)},i,u,p),w=Y(b,e,f,d,c,S(0,n),k),y=Y(b,e,f,d,c,p,k),z=G(b,c,v,o,t);return P(b,f,c,G(b,c,P(b,f,c,G(b,c,y,o,[0,b]),w),m,[0,b]),z)}}}function
b3(a,e,g,f,c,d){switch(d[0]){case
0:var
h=d[1];return[0,b(f,h,h)];case
1:var
l=d[1];return[1,l,b3(a,e,g,f,c,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=Y(a,e,g,f,c,k,S(0,b2(a,e,f,c,i,b(g,e,e)))),n=b3(a,e,g,f,c,i);return G(a,c,P(a,g,c,G(a,c,b3(a,e,g,f,c,k),j,[0,a]),m),j,n)}}function
fV(c,b,a){return fS(a,fU(c,b))}function
b4(h,g,f,e,d,c,n,b,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return a(c,Y(h,g,f,e,d,j,b));else{if(0===i[0]){var
k=i[1];return a(c,Y(h,g,f,e,d,b4(h,g,f,e,d,c,b4(h,g,f,e,d,c,j,b,k),b,k),b))}var
l=i[1],j=b4(h,g,f,e,d,c,j,b,l),i=l;continue}}function
fW(h,a,g,f,e,d,c,b){return b?b4(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
O(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return fV(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=O(a,f,c,g,e,d,b,j[1]);return z(a,c,e,d,b,O(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=O(a,f,c,g,e,d,b,i[1]);return z(a,c,e,d,b,O(a,f,c,g,e,d,b,j),l)}var
k=O(a,f,c,g,e,d,b,i);return P(a,c,b,O(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=O(a,f,c,g,e,d,b,h[2]);return z(a,c,e,d,b,O(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=O(a,f,c,g,e,d,b,h[2]);return Y(a,f,c,g,b,O(a,f,c,g,e,d,b,p),q);case
5:return ac(d,O(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=O(a,f,c,g,e,d,b,h[1]);return fW(a,f,c,g,b,function(a){return a},s,r)}}function
aM(c,b){if(typeof
b==="number")switch(b){case
0:return 0;case
1:return 1;default:return 2}else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:var
d=b[1],e=aM(c,b[2]);return[1,aM(c,d),e];case
2:var
f=b[1],g=aM(c,b[2]);return[2,aM(c,f),g];case
3:return[3,aM(c,b[1])];default:var
h=b[1],i=aM(c,b[2]);return[4,aM(c,h),i]}}var
cF=0;function
cH(e,d,c,f){if(f){var
h=f[2],g=f[1],i=b(d,c,g);if(i){if(a(e,i[1]))return 0;var
j=cH(e,d,c,h);return j?[0,[0,g,j[1]]]:0}var
k=cH(e,d,c,h);return k?[0,[0,g,k[1]]]:0}var
l=b(d,c,c);return l?a(e,l[1])?0:[0,[0,c,0]]:[0,[0,c,0]]}function
fX(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=cH(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
fY(e,d,c,a){var
b=0;return ey(function(f,a){var
b=fX(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
b5(d,c,a,b){if(a){var
e=a[2],f=fY(d,c,a[1],b);return bu(b5(d,c,e,b),f)}return cF}function
af(d,c,f,e,p,o){var
b=p,g=o;for(;;)if(typeof
g==="number")switch(g){case
0:return b?cF:cG;case
1:return b?cG:cF;default:return cG}else
switch(g[0]){case
0:var
h=g[1];return b?a(f,h):a(e,h);case
1:var
i=g[2],j=g[1];if(b){var
q=af(d,c,f,e,b,i);return bu(af(d,c,f,e,b,j),q)}var
r=af(d,c,f,e,b,i);return b5(d,c,af(d,c,f,e,b,j),r);case
2:var
k=g[2],l=g[1];if(b){var
s=af(d,c,f,e,b,k);return b5(d,c,af(d,c,f,e,b,l),s)}var
t=af(d,c,f,e,b,k);return bu(af(d,c,f,e,b,l),t);case
3:var
u=g[1],b=bt(b),g=u;continue;default:var
m=g[2],n=g[1];if(b){var
v=af(d,c,f,e,b,m);return b5(d,c,af(d,c,f,e,bt(b),n),v)}var
w=af(d,c,f,e,b,m);return bu(af(d,c,f,e,bt(b),n),w)}}function
fZ(f,e,d){var
c=e,a=d;for(;;){if(c){var
g=c[2],h=c[1];if(a){var
i=a[2];if(b(f,h,a[1])){var
c=g,a=i;continue}return 0}return 0}return 1}}function
cI(g,f,e,d,c,b,a){return fZ(c,af(g,f,e,d,1,b),a)}function
eE(d,c,a){return bt(b(d,c,a))}function
eF(f,e,c,a){var
d=b(e,c,a);return d?eE(f,c,a):d}function
f0(b,a){switch(b){case
0:return jl;case
1:return 1===a?jm:0===a?jn:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?jo:jp}}function
f1(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?jq:0;case
2:return 1===a?0:jr;default:return 1===a?0:0===a?js:[0,a]}}function
cJ(c,b){return b?a(c,b[1]):0}function
eG(d,c,a){if(c){var
e=c[1];return a?b(d,e,a[1]):0}return 0}function
f2(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,Y(g,f,e,d,c,b,h),0]]:0}function
f3(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=f0(b[2],a[2]);return cJ(function(a){return[0,[0,Y(g,f,e,d,c,h,i),a]]},j)}function
b6(e,d,c,b,a){var
f=b[1],g=a[1],h=f1(b[2],a[2]);return cJ(function(a){return[0,[0,P(e,d,c,f,g),a]]},h)}function
a6(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,fN(b[1],g,[0,[0,a],0])];case
1:return[0,[0,b3(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=a6(a,f,d,e,c,h,g,b[2]);return cJ(function(b){return f2(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=a6(a,f,d,e,c,h,g,b[2]),n=a6(a,f,d,e,c,h,g,l);return eG(function(b,g){return f3(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=a6(a,f,d,e,c,h,g,b[2]),q=a6(a,f,d,e,c,h,g,o);return eG(function(b,e){return b6(a,d,c,b,e)},q,p);default:var
i=b[1];return eF(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
b7(a,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
c=g[1];switch(h){case
0:return eE(d,c,a);case
1:return b(d,c,a);case
2:return b(f,c,a);default:return eF(d,f,c,a)}}return 0}function
cK(c,i,h,g,b,a,f,e){var
d=a6(c,i,h,g,b,a,f,e);return d?b7(c,b,a,d[1]):0}function
f4(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=O(e,j,d,i,c,b,a,h[1]),g=O(e,j,d,i,c,b,a,k);switch(l){case
0:var
m=[0,[0,z(e,d,c,b,a,g,f),2],0];return[0,[0,z(e,d,c,b,a,f,g),2],m];case
1:return[0,[0,z(e,d,c,b,a,f,g),0],0];case
2:return[0,[0,z(e,d,c,b,a,f,g),2],0];case
3:return[0,[0,z(e,d,c,b,a,g,f),2],0];case
4:return[0,[0,z(e,d,c,b,a,f,g),3],0];default:return[0,[0,z(e,d,c,b,a,g,f),3],0]}}function
eH(h,g,f,e,d,c,b,a){var
i=f4(h,g,f,e,d,c,b,a);return bI(function(a){return[0,a,0]},i)}function
f5(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=O(e,j,d,i,c,b,a,h[1]),g=O(e,j,d,i,c,b,a,k);switch(l){case
0:return[0,[0,z(e,d,c,b,a,f,g),0],0];case
1:var
m=[0,[0,z(e,d,c,b,a,g,f),2],0];return[0,[0,z(e,d,c,b,a,f,g),2],m];case
2:return[0,[0,z(e,d,c,b,a,g,f),3],0];case
3:return[0,[0,z(e,d,c,b,a,f,g),3],0];case
4:return[0,[0,z(e,d,c,b,a,g,f),2],0];default:return[0,[0,z(e,d,c,b,a,f,g),2],0]}}function
eI(h,g,f,e,d,c,b,a){var
i=f5(h,g,f,e,d,c,b,a);return bI(function(a){return[0,a,0]},i)}function
cL(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return[0,c[1]];case
1:var
g=c[2],d=b(x[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[2],j=c[1],k=cL(a(x[1],d),h);return[2,[4,cL(d,j),[6,[1,d],[0,i]]],k]}}function
jt(a){return cL(0,a)}function
au(c,b){switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1]];case
2:var
d=b[1],e=au(c,b[2]);return[2,au(c,d),e];case
3:var
f=b[1],g=au(c,b[2]);return[3,au(c,f),g];case
4:var
h=b[1],i=au(c,b[2]);return[4,au(c,h),i];case
5:return[5,au(c,b[1])];default:var
j=b[2];return[6,au(c,b[1]),j]}}function
f6(b,a){var
c=a[2],d=a[1],e=au(b,a[3]);return[0,au(b,d),c,e]}function
ju(q,h,f,g,c){if(typeof
c!=="number")switch(c[0]){case
1:var
m=c[1];if(0===m[0]){var
n=m[1];return b(g,q,n)?0:[5,b(f,n,n)]}return[1,m];case
3:var
a=c[2],d=c[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
a==="number"?0:5===a[0]?[3,[5,b(f,a[1],s)],i]:c}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
a==="number"?0:5===a[0]?[3,[5,b(f,a[1],r)],j]:c}return typeof
a==="number"?0:5===a[0]?b(g,h,a[1])?d:[3,d,a]:c;case
5:var
e=d[1];if(typeof
a==="number")return 0;else
switch(a[0]){case
3:var
k=a[2],l=a[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,b(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,b(f,e,k[1])],l];return b(g,h,e)?a:[3,d,a];case
4:return[4,[3,[5,e],a[1]],[3,[5,e],a[2]]];case
5:return[5,b(f,e,a[1])];default:return b(g,h,e)?a:[3,d,a]}default:return typeof
a==="number"?0:5===a[0]?b(g,h,a[1])?d:[3,d,a]:c}case
4:var
o=c[2],p=c[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return c}function
jv(a){return a[1]}function
jw(a){return a[2]}function
av(c,a){var
d=b(s[8],a[1],[0,c[2]]);return at(b(s[8],c[1],[0,a[2]]),d)}function
b8(c,a){var
d=b(s[8],a[1],[0,c[2]]),e=b(s[8],c[1],[0,a[2]]);return b(s[10],e,d)}function
aw(c,a){var
d=b(x[11],c[2],a[2]),e=b(s[8],a[1],[0,c[2]]),f=b(s[8],c[1],[0,a[2]]);return[0,b(s[5],f,e),d]}function
aN(c,a){var
d=b(x[11],c[2],a[2]);return[0,b(s[8],c[1],a[1]),d]}function
bz(b){var
c=b[2];return[0,a(s[6],b[1]),c]}function
bK(b,a){return aw(b,bz(a))}function
eJ(b){var
a=b[1];return typeof
a==="number"?jx:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
eK(a,b){return cC(aN,a,b)}function
jy(b,a){return typeof
a==="number"?jz:0===a[0]?eK(b,a[1]):eJ(eK(b,a[1]))}function
jA(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
bL(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,bL(b,a[1],c)]:[1,bL(b,a[1],c),b,0]}function
eL(d,a,b,c){if(typeof
c==="number")return bL(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,bL(d,a[1],b)]:[1,bL(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,eL(d,a[1],b,e)]:[1,eL(d,a[1],b,f),h,e]}}var
jB=s[10],jC=s[8],jD=s[5],jF=0;function
jG(a,b){return cK(jF,jE,jD,jC,at,jB,a,b)}var
jH=s[6],jI=s[7],jJ=s[5],jK=0;function
Z(a,b){return z(jK,jJ,jI,jH,at,a,b)}var
jL=s[5],jM=0;function
ax(a,b){return P(jM,jL,at,a,b)}var
jN=s[6],jO=s[7],jP=s[8],jQ=s[5],jS=0;function
b9(a){return O(jS,jR,jQ,jP,jO,jN,at,a)}function
f7(c){var
d=c[3],e=c[2],a=b9(c[1]),b=b9(d);switch(e){case
0:var
f=[0,[0,Z(b,ax(a,jT)),3],0];return[0,[0,Z(a,ax(b,jU)),3],f];case
1:return[0,[0,Z(a,b),0],0];case
2:return[0,[0,Z(a,ax(b,jV)),3],0];case
3:return[0,[0,Z(b,ax(a,jW)),3],0];case
4:return[0,[0,Z(a,b),3],0];default:return[0,[0,Z(b,a),3],0]}}function
f8(a){var
b=f7(a);return bI(function(a){return[0,a,0]},b)}function
f9(c){var
d=c[3],e=c[2],a=b9(c[1]),b=b9(d);switch(e){case
0:return[0,[0,Z(a,b),0],0];case
1:var
f=[0,[0,Z(b,ax(a,jX)),3],0];return[0,[0,Z(a,ax(b,jY)),3],f];case
2:return[0,[0,Z(b,a),3],0];case
3:return[0,[0,Z(a,b),3],0];case
4:return[0,[0,Z(b,ax(a,jZ)),3],0];default:return[0,[0,Z(a,ax(b,j0)),3],0]}}function
f_(a){var
b=f9(a);return bI(function(a){return[0,a,0]},b)}var
j1=s[10],j2=0;function
eM(a){return b7(j2,at,j1,a)}var
j3=s[5],j4=0;function
f$(a,b){return b6(j4,j3,at,a,b)}function
ga(e,d){var
a=b(s[17],e,d),c=a[1];return typeof
a[2]==="number"?c:b(s[5],c,j5)}function
eN(c,a){var
d=b(s[19],c,a);return b(s[13],d,j6)}function
b_(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=b_(a[1]),f=b[2],g=b[1],c=b_(e),h=c[2],i=c[1];return[0,eN(eN(g,f),i),h]}}function
b$(a,c){switch(a[0]){case
0:return[0,b(s[18],a[1],c)];case
1:var
d=a[1];return[1,d,b$(a[2],c)];default:var
e=a[2],f=a[1],g=b$(a[3],c);return[2,b$(f,c),e,g]}}function
cM(c){var
e=b_(c),f=e[2],d=e[1];if(b(s[12],d,0)){var
g=ga(a(s[6],f),d),h=a(s[6],g);return[0,b$(bJ(s[7],c,f),d),h]}return[0,c,0]}function
cN(d){var
e=d[2],a=d[1];switch(e){case
0:var
f=b_(a),g=f[2],c=f[1];if(b(s[12],c,0))if(bt(at(g,0)))if(bt(at(b(s[19],c,g),c)))return 0;return[0,[0,cM(a),0]];case
1:return[0,[0,[0,a,0],e]];case
2:return[0,[0,cM(bJ(s[7],a,j7)),3]];default:return[0,[0,cM(a),3]]}}function
gb(a){var
b=a[1],c=a[2];return[0,ax(b[1],[0,b[2]]),c]}function
gc(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
j8=s[10],j9=s[8],j_=s[5],ka=0;function
ca(a,b){return a6(ka,j$,j_,j9,at,j8,a,b)}function
eO(a){return 0===a?1:3<=a?1:0}function
eP(w,v){var
d=w,c=v;for(;;)if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
x=c[2],g=ca(d,c[1]);if(g){var
h=g[1];if(eM(h))return 1;var
d=[0,h,d],c=x;continue}return 0;case
1:var
y=c[2],i=ca(d,c[1]);if(i){var
j=cN(i[1]);if(j){var
d=[0,gb(j[1]),d],c=y;continue}return 1}return 0;default:var
z=c[3],A=c[2],k=ca(d,c[1]);if(k){var
B=k[1],l=ca(d,A);if(l){var
C=l[1],m=cN(B);if(m){var
n=m[1],o=n[1],p=o[1],D=n[2],E=o[2],q=cN(C);if(q){var
r=q[1],t=r[1],F=r[2],G=t[2],H=t[1];if(eO(D))if(eO(F))if(gc(ax(p,H))){var
f=z,e=a(s[6],E);for(;;){if(f){var
I=f[2],J=f[1],u=eP([0,[0,Z(p,[0,e]),0],d],J);if(u){var
f=I,e=b(s[5],e,kb);continue}return u}return b(s[12],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
kc(b,a){return cI(eM,f$,f8,f_,eP,b,a)}function
gd(a,b){return cK(ke,kd,aw,aN,av,b8,a,b)}function
ge(a){return eH(kg,kf,aw,aN,bK,bz,av,a)}function
gf(a){return eI(ki,kh,aw,aN,bK,bz,av,a)}function
gg(a){return b7(kj,av,b8,a)}function
gh(a,b){return b6(kk,aw,av,a,b)}function
kl(b,a){return cI(gg,gh,ge,gf,gd,b,a)}function
aB(a){if(typeof
a==="number")return 0===a?km:kn;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aB(a[2]);return aw(aB(b),c);case
3:var
d=a[1],e=aB(a[2]);return bK(aB(d),e);case
4:var
f=a[1],g=aB(a[2]);return aN(aB(f),g);case
5:return eJ(aB(a[1]));default:return bz(aB(a[1]))}}function
gi(a,b){return cK(kp,ko,aw,aN,av,b8,a,b)}function
gj(a){return eH(kr,kq,aw,aN,bK,bz,av,a)}function
gk(a){return eI(kt,ks,aw,aN,bK,bz,av,a)}function
gl(a){return b7(ku,av,b8,a)}function
gm(a,b){return b6(kv,aw,av,a,b)}var
p=[0,bt,bu,fI,eu,iP,x,iX,cC,fN,bI,ey,s,at,jj,jk,eB,S,fS,G,fT,fU,ac,a5,bJ,bZ,b0,eC,eD,P,z,b1,b2,aL,Y,b3,fV,b4,fW,O,aM,cF,cG,cH,fX,fY,b5,bu,af,fZ,cI,eE,eF,f0,f1,cJ,eG,f2,f3,b6,a6,b7,cK,O,z,P,f4,eH,f5,eI,cL,jt,au,f6,ju,jv,jw,av,b8,aw,aN,bz,bK,eJ,eK,jy,jA,bL,eL,jG,Z,ax,b9,f7,f8,f9,f_,eM,f$,ga,eN,b_,b$,cM,cN,gb,gc,ca,eO,eP,kc,gd,ge,gf,gg,gh,kl,aB,gi,gj,gk,gl,gm,function(b,a){return cI(gl,gm,gj,gk,gi,aM(function(a){return f6(aB,a)},b),a)}];aJ(597,p,"Micromega_plugin.Micromega");function
kw(e,c,d){var
a=d;for(;;){if(a){var
f=a[2];b(e,c,a[1]);b(l[55],c,kx);var
a=f;continue}return 0}}function
ky(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
kz(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1][1],d);if(c)return c;var
b=f;continue}return 0}}function
kA(g,e){var
c=0,a=e;for(;;){if(a){var
d=a[2],i=a[1],h=function(d){return function(c,a){return[0,b(g,d,a),c]}}(i),c=f(q[20],h,c,d),a=d;continue}return c}}function
kB(e,d){var
c=0,a=d;for(;;){if(a){var
h=a[2],i=a[1],g=function(d){return function(c,a){return[0,b(e,d,a),c]}}(i),c=f(q[20],g,c,a),a=h;continue}return c}}function
kC(g,f,e){var
c=f,a=e;for(;;){if(c){var
h=c[2],i=c[1];if(a){var
d=a[2];if(b(g,i,a[1])){var
c=h,a=d;continue}var
a=d;continue}return 0}return 1}}function
kD(g,b){function
c(e,b){var
c=e[2],d=e[1];if(d)return[0,d,[0,b,c]];var
f=a(g,b);return f?[0,[0,[0,f[1],b]],c]:[0,d,[0,b,c]]}return f(q[20],c,kE,b)}function
cO(b){return 2===b[0]?a(gn[3],b[1]):o[2]}function
cP(b){switch(b[0]){case
0:return a(o[36],b[1]);case
1:return b[1];default:return a(gn[2],b[1])}}function
kF(f){var
c=o[1],a=f;for(;;){if(a){var
d=a[2],e=cP(a[1]),c=b(o[17],c,e),a=d;continue}return 0===b(o[23],c,o[1])?o[2]:c}}function
kG(f){var
c=o[2],a=f;for(;;){if(a){var
j=a[2],e=cO(a[1]),d=b(o[17],c,e),g=b(o[15],c,d),h=b(o[15],e,d),i=b(o[10],g,h),c=b(o[10],d,i),a=j;continue}var
k=function(a){var
d=cO(a),e=cP(a),f=b(o[10],e,c);return b(o[15],f,d)};return b(q[17],k,f)}}function
go(a){return a?go(a[1])+1|0:0}function
cQ(a){return typeof
a==="number"?1:0===a[0]?1+(2*cQ(a[1])|0)|0:2*cQ(a[1])|0}function
kH(a){return a?cQ(a[1]):0}function
eQ(a){return typeof
a==="number"?1:0===a[0]?1+(2*eQ(a[1])|0)|0:2*eQ(a[1])|0}function
cR(a){if(typeof
a==="number")return o[2];else{if(0===a[0]){var
c=cR(a[1]),d=b(o[11],2,c);return b(o[7],1,d)}var
e=cR(a[1]);return b(o[11],2,e)}}function
eR(b){if(typeof
b==="number")return o[1];else{if(0===b[0])return cR(b[1]);var
c=cR(b[1]);return a(o[3],c)}}function
kI(a){var
c=a[1],d=[1,eR([0,a[2]])],f=[1,eR(c)];return b(e[9],f,d)}function
gp(a){return 0===a?0:[0,gp(a-1|0)]}function
bM(a){return 1===a?0:1===(a&1)?[0,bM(a>>>1|0)]:[1,bM(a>>>1|0)]}function
kJ(a){if(0<=a)return 0===a?0:[0,bM(a)];throw[0,ay,kK]}function
eS(a){return 1===a?0:1===(a&1)?[0,eS(a>>>1|0)]:[1,eS(a>>>1|0)]}function
kL(a){var
b=ab(a,0);return 0===b?0:1===b?[0,bM(a)]:[1,bM(-a|0)]}function
cS(d){var
f=a(o[36],2);function
c(a){if(b(o[24],a,o[2]))return 0;var
d=b(o[14],a,f),e=d[1];return b(o[24],o[2],d[2])?[0,c(e)]:[1,c(e)]}return c(d)}function
gq(b){var
c=a(o[22],b);return 0===c?0:1===c?[0,cS(b)]:[1,cS(a(o[3],b))]}function
kM(a){var
b=cS(cO(a));return[0,gq(cP(a)),b]}function
kN(d){var
b=d;for(;;){if(b){var
e=b[2],c=a(b[1],0);if(0===c){var
b=e;continue}return c}return 0}}function
kO(g,f,e){var
c=f,a=e;for(;;){if(c){if(a){var
h=a[2],i=c[2],d=b(g,c[1],a[1]);if(0===d){var
c=i,a=h;continue}return d}return 1}return a?-1:0}}function
kP(a){return a}function
kQ(a){return a+1|0}function
kR(d,c){var
e=a(l[22],c);return b(l[55],d,e)}var
E=a(gr[1],[0,ab]);function
kS(a){for(;;)try{var
d=b(C[15],0,a)[2];return d}catch(a){a=w(a);if(a[1]===C[1]){var
c=a[2];if(typeof
c==="number")if(11===c)continue}throw a}}function
kT(c,s,p){var
e=b(C[67],0,0),g=e[2],h=e[1],i=b(C[67],0,0),j=i[2],k=i[1],m=b(C[67],0,0),n=m[2],t=m[1],u=V(C[69],c,s,h,j,n),o=a(C[31],g);b(l[61],o,p);a(l[52],o);var
d=kS(u);function
v(e){var
c=[0,h,[0,g,[0,k,[0,j,[0,t,[0,n,0]]]]]];function
d(b){try{var
c=a(C[24],b);return c}catch(a){return 0}}return b(q[15],d,c)}return ky(function(o){switch(d[0]){case
0:var
b=d[1];if(0===b){var
e=a(C[30],k);try{var
i=a(cT[3],e);return i}catch(b){b=w(b);var
g=a(eT[1],b),h=f(r[4],kU,c,g);return a(l[3],h)}}var
j=f(r[4],kV,c,b);return a(l[3],j);case
1:var
m=f(r[4],kW,c,d[1]);return a(l[3],m);default:var
n=f(r[4],kX,c,d[1]);return a(l[3],n)}},v)}var
k=[0,cP,cO,[0,kO,kN],[0,kR,kQ,kP],[0,E[1],E[2],E[3],E[4],E[5],E[6],E[7],E[8],E[9],E[10],E[11],E[12],E[13],E[15],E[16],E[17],E[18],E[19],E[20],E[21],E[22],E[24],E[26],E[28]],kw,[0,bM,gq,kJ,gp,kM,eS,kL,cS],[0,eR,kI,cQ,kH,go,eQ],kG,kB,kA,kz,kC,kF,kD,kT];aJ(606,k,"Micromega_plugin.Mutils");var
gs=e[2],bN=e[7],T=a(cU[1],[0,ab]),bO=T[1];function
gt(a){var
b=0;function
c(c,b,a){return a+b|0}return f(T[13],c,a,b)}function
cV(b,a){var
c=gt(b),d=gt(a);return c===d?f(T[10],ab,b,a):ab(c,d)}function
cb(a){return aj(a,T[1])}function
gu(a){try{var
b=1,c=function(c,b,a){if(1===a){if(1===b)return 0;throw J}throw J},d=1-f(T[13],c,a,b);return d}catch(a){a=w(a);if(a===J)return 0;throw a}}function
kY(a){if(cb(a))return 0;try{var
b=function(c,b,e){var
d=b/2|0;if(0===(b%2|0))return f(T[4],c,d,a);throw J},c=[0,f(T[13],b,a,bO)];return c}catch(a){a=w(a);if(a===J)return 0;throw a}}function
eU(c,a){try{var
d=b(T[27],c,a);return d}catch(a){a=w(a);if(a===J)return 0;throw a}}function
cW(b,a){function
c(b,c,a){var
d=eU(b,a)+c|0;return f(T[4],b,d,a)}return f(T[13],c,b,a)}function
gv(c,a){var
e=l[8];function
g(e,d,a){var
f=D.caml_div(eU(e,c),d);return b(l[5],a,f)}var
d=f(T[13],g,a,e),h=T[1];function
i(c,g,b){var
e=g-el(eU(c,a),d)|0;return 0===e?b:f(T[4],c,e,b)}return[0,f(T[13],i,c,h),d]}var
kZ=T[13],_=a(cU[1],[0,cV]);function
gw(c,a){try{var
d=b(_[27],c,a);return d}catch(a){a=w(a);if(a===J)return k0;throw a}}function
k1(b){var
c=_[1],a=f(T[4],b,1,T[1]);return f(_[4],a,k2,c)}function
gx(a){return f(_[4],bO,a,_[1])}function
gy(d,g,c){if(0===a(e[25],g))return c;var
h=b(gs,gw(d,c),g);return 0===a(e[25],h)?b(_[7],d,c):f(_[4],d,h,c)}function
gz(b,a){function
c(c,b,a){return gy(c,b,a)}return f(_[13],c,b,a)}function
k4(c,i){var
d=_[1];function
g(k,c,j){if(0===a(e[25],c))var
d=gx(k3);else
var
g=_[1],h=function(e,d,a){var
g=b(bN,c,d),h=cW(k,e);return f(_[4],h,g,a)},d=f(_[13],h,i,g);return gz(d,j)}return f(_[13],g,c,d)}function
k5(c){function
d(b){return a(e[3],b)}return b(_[33],d,c)}var
gA=_[13];function
k6(a){var
b=1;function
c(b,e,a){if(a){var
c=cb(b);if(!c)return gu(b);var
d=c}else
var
d=a;return d}return f(_[13],c,a,b)}function
k7(k,j){var
c=k,a=j;for(;;){if(c){var
d=c[1],l=c[2],m=d[2],n=d[1];if(a){var
f=a[1],g=n===f[1]?1:0,o=a[2],p=f[2];if(g){var
h=b(e[26],m,p);if(h){var
c=l,a=o;continue}var
i=h}else
var
i=g;return i}return 0}return a?0:1}}function
k8(f){var
c=0,b=f;for(;;){if(b){var
d=b[1],g=b[2],h=d[1],i=[0,h,a(e[56],d[2])],c=c+a(bA[27],i)|0,b=g;continue}return a(bA[27],c)}}var
k9=0;function
eV(g,c){function
d(b){var
c=b[1],d=a(e[40],b[2]);return f(r[2],k_,d,c)}return b(q[15],d,c)}function
k$(a){function
d(i,h){var
c=i,a=h;for(;;){if(a){var
f=a[2],g=a[1];if(b(e[31],g,la))return[0,[0,c,g],d(c+1|0,f)];var
c=c+1|0,a=f;continue}return 0}}return d(0,a)}function
lb(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,eW,b(c+1|0,a)]}return 0}return b(0,a)}function
bP(d,c,a){return b(e[26],c,lc)?a:[0,[0,d,c],a]}function
gB(d,c,b){if(b){var
f=b[2],g=b[1],h=g[2],e=g[1],i=ab(d,e)+1|0;if(2<i>>>0)return a(l[3],ld);switch(i){case
0:return bP(d,a(c,eW),b);case
1:return bP(e,a(c,h),f);default:return[0,[0,e,h],gB(d,c,f)]}}return bP(d,a(c,eW),0)}function
gC(d,c,b){if(b){var
f=b[2],g=b[1],e=g[1],h=ab(d,e)+1|0,i=g[2];if(2<h>>>0)return a(l[3],le);switch(h){case
0:return bP(d,c,b);case
1:return bP(e,c,f);default:return[0,[0,e,i],gC(d,c,f)]}}return bP(d,c,0)}function
eX(a,c){if(0===a[0]){var
d=a[1];if(0===d)return 0;if(1===d)return c}function
f(c){var
d=c[1];return[0,d,b(e[7],a,c[2])]}return b(q[17],f,c)}function
cc(o,n){var
c=o,a=n;for(;;){if(c){if(a){var
f=a[2],i=a[1],j=i[2],g=i[1],h=c[2],k=c[1],l=k[2],d=k[1];if(aj(d,g)){var
m=b(e[1],l,j);if(b(e[26],m,lf)){var
c=h,a=f;continue}return[0,[0,d,m],cc(h,f)]}return fv(d,g)?[0,[0,d,l],cc(h,a)]:[0,[0,g,j],cc(c,f)]}return c}return a?a:0}}function
lg(d,c){var
f=0,g=[0,function(a){return b(e[37],d[2],c[2])},f],h=[0,function(a){return ab(d[1],c[1])},g];return a(k[3][2],h)}var
lh=a(k[3][1],lg);function
gD(h,g){var
a=g;for(;;){if(a){var
c=a[1],d=ab(c[1],h),e=a[2],f=c[2];if(-1===d){var
a=e;continue}var
b=0===d?[0,[0,f,a]]:0}else
var
b=0;return b?[0,b[1][1]]:0}}function
li(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}function
ll(c,b){var
d=b[2],f=b[1],g=a(e[40],b[3]),h=0===d?lj:lk;return M(r[1],c,lm,eV,f,h,g)}function
ln(b,a){if(0===b){if(0===a)return 0}else
if(0!==a)return 1;return 1}function
aC(d,c){if(typeof
c==="number")return b(r[1],d,lo);else
switch(c[0]){case
0:return f(r[1],d,lp,c[1]);case
1:return f(r[1],d,lq,c[1]);case
2:var
e=a(o[33],c[1]);return f(r[1],d,lr,e);case
3:return b(r[1],d,ls);case
4:return aa(r[1],d,lt,aC,c[2]);case
5:var
g=c[2],h=a(o[33],c[1]);return V(r[1],d,lu,aC,g,h);case
6:return M(r[1],d,lv,aC,c[1],aC,c[2]);case
7:return M(r[1],d,lw,aC,c[1],aC,c[2]);default:return aa(r[1],d,lx,aC,c[1])}}function
eY(d,c){if(typeof
c==="number")return b(r[1],d,ly);else{if(0===c[0])return fw(r[1],d,lz,c[1],aC,c[2],eY,c[3]);var
e=c[5],f=c[4],g=c[3],h=c[2],i=c[1],j=a(k[6],eY);return uM(r[1],d,lA,i,aC,h,eV,g,aC,f,j,e)}}function
cd(e){var
a=e;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
8:var
d=a[1],c=1;break;case
0:case
1:return a[1];case
4:case
5:var
d=a[2],c=1;break;case
6:case
7:var
f=a[1],g=cd(a[2]),h=cd(f);return b(l[6],h,g);default:var
c=0}if(c){var
a=d;continue}return-1}}function
eZ(a){if(typeof
a==="number")return-1;else{if(0===a[0]){var
c=a[2],d=a[1],e=eZ(a[3]),g=cd(c),h=b(l[6],g,e);return b(l[6],d,h)}var
i=a[5],j=a[2],k=a[1],m=cd(a[4]),n=cd(j),o=b(l[6],n,m),p=b(l[6],k,o),r=function(c,a){var
d=eZ(a);return b(l[6],c,d)};return f(q[20],r,p,i)}}function
aO(c,a){if(typeof
a!=="number")switch(a[0]){case
4:var
n=a[1],d=aO(c,a[2]);return[0,d[1],d[2],[4,n,d[3]]];case
5:var
e=aO(c,a[2]),f=e[2];return[0,[0,[0,f,e[3]],e[1]],f+1|0,[1,f]];case
6:var
o=a[2],g=aO(c,a[1]),p=g[3],q=g[1],h=aO(g[2],o),r=h[2],s=[6,p,h[3]];return[0,b(l[26],h[1],q),r,s];case
7:var
t=a[2],i=aO(c,a[1]),u=i[3],v=i[1],j=aO(i[2],t),w=j[2],x=[7,u,j[3]];return[0,b(l[26],j[1],v),w,x];case
8:var
k=aO(c,a[1]),m=k[2];return[0,[0,[0,m,k[3]],k[1]],m+1|0,[1,m]]}return[0,0,c,a]}function
e0(c,a){if(typeof
a!=="number"&&8===a[0]){var
b=aO(c,a[1]);return[0,b[1],b[2],[8,b[3]]]}return aO(c,a)}function
gE(b){var
a=b;for(;;){if(typeof
a!=="number"&&8===a[0]){var
a=a[1];continue}return a}}function
e1(e,n){var
c=n;for(;;)if(typeof
c==="number")return[0,e,0];else{if(0===c[0]){var
d=c[2],j=c[1];if(typeof
d!=="number"&&5===d[0])if(typeof
c[3]==="number"){var
c=[0,j,d[2],0];continue}var
o=c[3],g=e0(e,d),p=g[3],r=g[1],k=e1(g[2],o),s=k[1],t=[0,j,p,k[2]],u=function(b,a){return[0,a[1],[8,a[2]],b]};return[0,s,f(q[20],u,t,r)]}var
v=c[5],w=c[4],x=c[3],y=c[1],h=e0(e,gE(c[2])),z=h[3],A=h[2],B=h[1],i=e0(A,gE(w)),C=i[3],D=i[2],E=i[1],F=function(a){return e1(D,a)},G=b(q[17],F,v),m=a(q[46],G),H=m[2],I=m[1],J=b(l[26],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[8,a[2]],b]},M=f(q[20],L,K,J);return[0,f(q[20],l[6],0,I),M]}}function
lB(b,a){return e1(b,a)}function
gF(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[7,b,a];var
c=b}return c}function
e2(c,d){var
e=a(o[22],c)+1|0;if(2<e>>>0)throw[0,ay,lC];switch(e){case
0:return[4,[0,0,[1,c]],d];case
1:return 0;default:return b(o[24],c,o[2])?d:[6,[2,c],d]}}var
cX=a(cU[1],[0,cV]),cY=a(cU[1],[0,ab]),cZ=[0,cX[1]],c0=[0,cY[1]],e3=[0,0];function
lD(a){cZ[1]=cX[1];c0[1]=cY[1];e3[1]=0;return 0}function
c1(c){try{var
a=b(cX[27],c,cZ[1]);return a}catch(a){a=w(a);if(a===J){var
d=e3[1];cZ[1]=f(cX[4],c,d,cZ[1]);c0[1]=f(cY[4],d,c,c0[1]);e3[1]++;return d}throw a}}function
c2(a){return b(cY[27],a,c0[1])}function
gG(a){var
c=a[2],d=a[1];function
e(b,a){return ab(b[1],a[1])}return[0,b(q[48],e,d),c]}function
lE(b){var
a=f(gA,function(c,b,a){var
d=a[1],e=a[2];return cb(c)?[0,d,b]:[0,[0,[0,c1(c),b],d],e]},b,lF);return gG([0,a[1],a[2]])}var
n=[0,[0,kZ,bO,kY,gu,gv,cV],[0,gx,k1,gz,k4,k5,gw,gA,k6,gy],[0,k8,k7,lh,eV,gD,gC,li,gB,k9,k$,lb,cc,eX],eZ,lB,eY,gF,e2,[0,[0,lD,c2],function(I,c){var
J=c[2],w=c[1],d=gD(I,w[1]);if(d){var
K=d[1];return function(L){var
A=L[1],S=L[2],T=A[2],B=c2(I);function
E(b,a){var
c=a[2],d=b[2],e=a[1],f=b[1];return fv(d,c)?-1:aj(d,c)?cV(f,e):1}var
r=[0,bO,l[8]],p=[0,A,S];a:for(;;){var
s=p[2],g=p[1],ac=r[2],ad=r[1],Z=g[1],_=[0,lJ,bO,0],$=function(a,b){var
c=a[3],d=a[2],h=b[2],i=a[1],e=gv(c2(b[1]),B),f=e[2],g=e[1];return-1===E([0,d,c],[0,g,f])?[0,h,g,f]:[0,i,d,c]},o=f(q[20],$,_,Z),F=o[3],aa=o[2],ab=o[1],G=0<F?[0,[0,ab,aa,F]]:0;if(G){var
t=G[1],u=t[3],v=t[2],ae=t[1];if(-1===E([0,v,u],[0,ad,ac])){var
m=a(e[15],K),c=b(bN,[0,-a(e[25],K)|0],ae),j=bO,i=u-1|0;for(;;){if(0===i){var
d=cW(v,j),n=a(e[3],w[3]),C=w[1];if(cb(d))var
M=b(bN,c,n),h=[0,eX(c,C),M];else
if(0===a(e[25],c))var
h=lG;else{if(0===a(e[25],n))var
z=0;else
var
P=b(bN,c,n),z=[0,[0,c1(d),P],0];var
N=function(e,f){return function(a){var
c=a[2],d=c1(cW(f,c2(a[1])));return[0,d,b(bN,e,c)]}}(c,d),O=b(q[17],N,C),h=gG([0,b(l[26],z,O),lH])}var
Q=h[2],R=h[1],U=b(bN,m,g[3]),V=b(gs,a(e[3],Q),U),W=[0,cc(eX(m,g[1]),R),T,V],X=e2(a(k[1],m),s),D=cb(d)?[0,0,c]:[0,[0,[0,c1(d),c],0],lI],x=D[2],y=D[1],Y=y?[4,[0,y,x],J]:e2(a(k[1],x),J),r=[0,v,u],p=[0,W,gF(Y,X)];continue a}var
j=cW(j,B),i=i-1|0;continue}}var
H=[0,g,s]}else
var
H=[0,g,s];return[0,H]}}}return function(a){return 0}},lE],ll,ln];aJ(611,n,"Micromega_plugin.Polynomial");function
gH(a){var
c=a[1];if(c){var
d=a[2];if(d)return b(e[29],c[1],d[1])?[0,a]:0}return[0,a]}function
gI(c,a){var
f=a[2],g=a[1],h=c[2],i=c[1];function
d(d,c,a){if(c){var
e=c[1];return a?[0,b(d,e,a[1])]:c}return a?a:0}var
j=d(e[39],h,f);return gH([0,d(e[38],i,g),j])}function
e4(c){var
d=c[1];if(d){var
f=c[2];if(f){var
g=f[1],h=a(e[24],d[1]),i=a(e[22],g),j=b(e[4],i,h);return[0,b(e[1],j,lK)]}}return 0}function
lL(f,d){var
a=e4(f),c=e4(d);return a?c?b(e[29],a[1],c[1]):1:0}function
gJ(d,a){var
c=d[2],f=d[1];if(f){var
g=f[1];if(c){var
i=c[1],h=b(e[29],g,a);return h?b(e[29],a,i):h}return b(e[29],g,a)}return c?b(e[29],a,c[1]):1}var
ce=a(m[20][1],[0,ab]),gK=n[3],ak=a(bA[25],[0,gK[2],gK[1]]),gL=[0,l[8]],cf=[bs,lM,bq(0)];function
cg(b,a){switch(a[0]){case
0:return f(r[1],b,lN,a[1]);case
1:return fw(r[1],b,lO,a[1],cg,a[2],cg,a[3]);default:return M(r[1],b,lP,cg,a[1],cg,a[2])}}function
lQ(b,a){var
c=b[4],d=b[3],f=a[4],g=a[2],h=a[1],i=b[2],j=b[1];if(d===a[3])if(c===f){var
e=gI(j,h);return e?[0,[0,e[1],[2,i,g],d,c]]:0}throw[0,ay,lR]}function
lS(e,c,d){try{var
a=b(ak[7],d,e),g=lQ(c,a[1]);if(g){a[1]=g[1];var
h=0;return h}throw[0,cf,[2,c[2],a[1][2]]]}catch(a){a=w(a);if(a===J)return f(ak[10],d,e,[0,c]);throw a}}var
gM=[bs,lT,bq(0)];function
e5(d,c,b){var
e=gL[1];if(a(ak[15],b)<e)return lS(d,c,b);throw gM}function
e6(d,c){var
j=gH(c[1]);if(j){var
k=j[1],h=k[2],i=k[1];if(d){var
f=d[1][2],g=function(a){return b(e[9],a,f)};if(1===a(e[25],f))var
o=c[4],p=c[3],q=c[2],r=b(al[16],g,h),l=[0,[0,b(al[16],g,i),r],q,p,o];else
var
t=c[3],u=c[4],v=c[2],w=b(al[16],g,i),l=[0,[0,b(al[16],g,h),w],v,u,t];if(b(e[31],f,lU))var
s=function(a){var
c=a[1];return[0,c,b(e[9],a[2],f)]},n=b(m[17][69],s,d);else
var
n=d;return[0,n,l]}return gJ([0,i,h],lV)?1:0}return 0}function
lW(a){return 0===a?e[26]:e[30]}function
gN(h){var
d=0,c=0,b=h;for(;;){if(b){var
f=b[2],g=a(e[25],b[1][2]);if(0===g)throw[0,ay,lX];if(1===g){var
c=c+1|0,b=f;continue}var
d=d+1|0,b=f;continue}return[0,d,c]}}function
gO(a,e){var
b=a[3],c=a[1],f=a[2],d=gN(c),g=d[2],h=d[1],i=[0,e],j=0===f?[0,[0,b],[0,b]]:[0,[0,b],0];return e6(c,[0,j,i,g,h])}function
lY(d){var
c=a(ak[1],1000);function
e(b,a){return[0,a,b]}var
g=b(m[17][13],e,d),h=ce[1];function
i(e,d){var
g=d[2],h=d[1],a=gO(h,g);if(typeof
a==="number"){if(0===a)throw[0,cf,[0,g]];return e}e5(a[1],a[2],c);var
i=h[1];function
j(c,a){return b(ce[4],a[1],c)}return f(m[17][15],j,e,i)}return[0,c,f(m[17][15],i,h,g)]}function
gP(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return f(ak[14],d,b,c)}function
e7(f,c){var
g=c[2],h=f[2],i=c[1],j=f[1];if(b(e[31],h,lZ))if(b(e[31],g,l0)){var
d=function(s,r){var
c=s,a=r;for(;;){if(c){if(a){var
i=a[2],l=a[1],n=l[2],j=l[1],k=c[2],o=c[1],p=o[2],f=o[1];if(f===j){var
t=b(e[9],n,g),u=b(e[9],p,h),q=b(e[1],u,t);if(b(e[26],q,l1)){var
c=k,a=i;continue}return[0,[0,f,q],d(k,i)]}if(f<j){var
v=d(k,a);return[0,[0,f,b(e[9],p,h)],v]}var
w=d(c,i);return[0,[0,j,b(e[9],n,g)],w]}var
x=function(a){var
c=a[1];return[0,c,b(e[9],a[2],h)]};return b(m[17][69],x,c)}if(a){var
y=function(a){var
c=a[1];return[0,c,b(e[9],a[2],g)]};return b(m[17][69],y,a)}return 0}},a=d(j,i);return[0,a,gN(a)]}throw[0,ay,l2]}function
l3(i,c){var
h=c[1];function
j(k,t,j){var
c=t[1],f=j[3],g=j[2],h=j[1],l=b(n[3][5],i,k);if(l){var
m=l[1],d=function(b,a){return a?[0,[0,m,k,[0,[0,[0,a[1]],0],c[2],c[3],c[4]]],b]:b},o=c[1],p=o[2],q=o[1];if(1===a(e[25],m)){var
r=d(f,p);return[0,d(h,q),g,r]}var
s=d(f,q);return[0,d(h,p),g,s]}return[0,h,[0,[0,k,c],g],f]}var
d=f(ak[14],j,h,l4),k=d[3],l=d[2],o=d[1],p=a(ak[15],c[1]),g=a(ak[1],p);function
q(a){return f(ak[10],g,a[1],[0,a[2]])}b(m[17][11],q,l);function
r(d){function
c(f){var
h=f[3],j=f[1],k=d[3],l=d[1],p=f[2],q=d[2],r=h[1],s=a(al[7],k[1][1]),t=a(al[7],r[1]),u=a(e[3],j),v=b(e[9],t,u),w=b(e[9],s,l),x=b(e[1],w,v),m=e7([0,q,l],[0,p,a(e[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,i,k[2],h[2]],n[2],n[1]],c=e6(m[1],o);if(typeof
c==="number"){if(0===c)throw[0,cf,o[2]];return 0}return e5(c[1],c[2],g)}return b(m[17][11],c,k)}b(m[17][11],r,o);return[0,g,b(ce[6],i,c[2])]}function
l6(c,r,D,C,d){var
f=b(n[3][5],c,r),s=a(al[7],f),g=a(ak[15],d[1]),t=a(ak[1],g),h=d[1];function
i(f,E){var
g=E[1],i=b(n[3][5],c,f);if(i)var
j=i[1],k=b(e[30],j,l5)?a(e[3],s):s,l=a(e[15],j),m=e7([0,r,k],[0,f,l]),o=m[2],v=o[2],w=o[1],x=m[1],y=b(e[9],D,k),p=function(a){var
c=b(e[9],a,l);return b(e[1],y,c)},q=g[1],z=q[1],A=b(al[16],p,q[2]),B=[0,b(al[16],p,z),A],h=[0,x,[0,B,[1,c,C,g[2]],v,w]];else
var
h=[0,f,g];var
u=h[2],d=e6(h[1],u);if(typeof
d==="number"){if(0===d)throw[0,cf,u[2]];return 0}return e5(d[1],d[2],t)}b(ak[12],i,h);return[0,t,b(ce[6],c,d[2])]}var
A=a(l7[1],[0,ab]);function
gQ(u,t,c){function
d(y,x,v){function
g(l,d,k){var
c=l,a=k;for(;;){if(c){var
f=c[2],h=c[1],i=h[2],j=h[1];try{var
m=b(A[22],j,u),n=b(e[6],m,i),o=g(f,b(e[1],d,n),a);return o}catch(b){b=w(b);if(b===J){var
c=f,a=[0,[0,j,i],a];continue}throw b}}return[0,d,a]}}var
h=g(y,l8,0),m=h[1],o=b(n[3][5],t,h[2]),p=o?o[1]:mb,d=x[1][1];function
c(a){var
c=b(e[4],a,m);return b(e[9],c,p)}var
i=d[2],j=d[1],k=a(e[25],p);if(0===k)var
f=gJ(d,m)?l9:a(l[3],l_);else
if(1===k)var
r=b(al[16],c,i),f=[0,b(al[16],c,j),r];else
var
s=b(al[16],c,j),f=[0,b(al[16],c,i),s];var
q=gI(v,f);return q?q[1]:a(l[3],ma)}return f(ak[14],d,c,l$)}function
mk(g,k,j,d,c){function
e(c,f){var
l=a(k,c);try{var
q=function(a){return a[1][1]!==g?1:0},d=b(m[17][27],q,l)[1],i=d[1],r=e(l6(i,d[2],d[3],d[4],c),[0,[0,i,c],f]);return r}catch(d){d=w(d);if(d===J){var
n=a(j,c);try{var
o=function(a){return a[1]!==g?1:0},h=b(m[17][27],o,n)[1],p=e(l3(h,c),[0,[0,h,c],f]);return p}catch(a){a=w(a);if(a===J)return[0,[0,c,f]];throw a}}throw d}}return e(d,c)}function
gR(d,c,b,a){try{var
e=mk(d,c,b,lY(a),0);return e}catch(a){a=w(a);if(a[1]===cf)return[1,a[2]];throw a}}function
gS(c){var
d=c[2],g=[0,0,gP(c)];function
h(w,v){var
d=v[2],c=0,h=0,i=0,g=0,F=v[1];for(;;){if(d){var
j=d[2],n=d[1],b=n[2],k=n[1];if(k){var
o=k[2],p=k[1],x=p[2];if(w===p[1]){var
l=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(b),q=b[1],r=q[2],s=q[1];if(1===a(e[25],x)){var
y=l(g,r),d=j,c=[0,[0,o,b],c],h=l(h,s),g=y;continue}var
z=l(g,s),d=j,c=[0,[0,o,b],c],h=l(h,r),g=z;continue}var
d=j,c=[0,[0,k,b],c],i=(b[4]+b[3]|0)+i|0;continue}var
d=j,c=[0,[0,0,b],c],i=(b[4]+b[3]|0)+i|0;continue}var
t=a(m[17][1],h),A=0,B=function(b,a){return b+a|0},C=f(m[17][15],B,A,h),u=a(m[17][1],g),D=0,E=function(b,a){return b+a|0};return[0,[0,[0,w,i+u*C+t*f(m[17][15],E,D,g)-u*t],F],c]}}var
i=f(ce[15],h,d,g)[1];function
j(b,a){return D.caml_float_compare(b[2],a[2])}return b(m[17][39],j,i)}function
gT(a){var
c=a[1];if(c){var
d=a[2];if(d)return b(e[26],c[1],d[1])}return 0}function
gU(b,e){var
a=e;for(;;){if(a){var
c=a[2],d=a[1][1];if(d===b)return[0,1,c];if(d<b){var
a=c;continue}return[0,0,a]}return ml}}function
gV(E){var
o=gP(E),F=0;function
G(c,d){var
a=d[2],f=a[1],g=f[1],j=d[1];if(g){var
h=f[2];if(h){var
i=g[1];return b(e[26],i,h[1])?[0,[0,j,i,a[2],a[4]+a[3]|0],c]:c}}return c}var
p=f(m[17][15],G,F,o),a=p;for(;;){if(a){var
c=a[1],d=c[1],t=a[2],u=c[4],v=c[3],w=c[2];if(d)if(d[2])var
q=0;else
var
n=[0,[0,d[1][1],d,w,v,u]],q=1;else
var
q=0;if(!q){var
a=t;continue}}else
var
n=0;if(n)var
j=[0,n[1]];else{var
h=p;b:for(;;){if(h){var
i=h[1],s=i[1],g=s,A=h[2],B=i[4],C=i[3],D=i[2];for(;;){if(g){var
r=g[1][1],z=g[2],x=0,y=function(d){return function(a,b){var
c=b[2];return gU(d,b[1])[1]?gT(c[1])?a+1|0:a:a}}(r);if(2!==f(m[17][15],y,x,o)){var
g=z;continue}var
l=[0,r]}else
var
l=0;if(!l){var
h=A;continue b}var
j=[0,[0,l[1],s,D,C,B]];break}}else
var
j=0;break}}if(j){var
k=j[1];return[0,[0,[0,k[1],k[2],k[3],k[4]],0],0]}var
H=0,I=function(s,f){var
p=f[1],e=p,m=o,j=s,t=f[4],u=f[3],v=f[2];a:for(;;){if(e){var
n=e[1][1],c=m,b=0,a=0,q=e[2],r=t-1|0;for(;;){if(c){var
g=c[2],k=c[1],d=k[2],h=d[3]+d[4]|0,l=gU(n,k[1]),i=l[2];if(0===l[1]){var
c=g,b=b+h|0,a=[0,[0,i,d],a];continue}if(gT(d[1])){var
c=g,b=b+h|0,a=[0,[0,i,d],a];continue}var
c=g,b=(b+h|0)+r|0,a=[0,[0,i,d],a];continue}var
e=q,m=a,j=[0,[0,[0,n,p,v,u],b],j];continue a}}return j}},J=f(m[17][15],I,H,p),K=function(b,a){return ab(b[2],a[2])};return b(m[17][39],K,J)}}function
mm(g,d){var
h=0;function
i(d,c){var
e=a(n[3][7],c[1]);return b(l[1][5],d,e)}var
c=f(m[17][15],i,h,d),e=gR(c,gV,gS,[0,[0,f(n[3][6],c,mo,g),0,mn],d]);if(0===e[0]){var
j=e[1][1];try{var
o=[0,gQ(A[1],c,j[1])];return o}catch(c){c=w(c);if(a(a7[18],c)){var
k=a(eT[1],c);b(r[2],mp,k);return 0}throw c}}return 0}function
mq(x){var
i=gR(l[8],gV,gS,x);if(0===i[0]){var
h=i[1][2],g=A[1];for(;;){if(h){var
s=h[1],t=s[1],y=h[2],j=gQ(g,t,s[2][1]),n=j[1];if(n){var
o=j[2],c=n[1];if(o){var
p=o[1];if(b(e[29],c,mc))if(b(e[29],md,p))var
d=me,k=1;else
var
k=0;else
var
k=0;if(!k)var
u=a(e[22],p),v=a(e[24],c),d=b(e[29],v,u)?a(e[24],c):c}else
var
d=b(e[29],c,mf)?mg:a(e[24],c)}else{var
q=j[2];if(q)var
r=q[1],w=a(e[22],r),d=b(e[29],mh,w)?mi:a(e[22],r);else
var
d=mj}var
h=y,g=f(A[4],t,d,g);continue}var
z=0,B=function(c,b,a){return[0,[0,c,b],a]},C=f(A[11],B,g,z);return[0,a(m[17][9],C)]}}return[1,i[1]]}function
bB(b,a){return e7(b,a)[1]}function
c3(b,a){if(0===b)if(0===a)return 0;return 1}function
gW(s,r,q){var
j=q[2],k=q[1],l=r[2],m=r[1],o=j[3],f=j[2],g=j[1],p=l[3],h=l[2],i=l[1],t=b(n[3][5],s,i),u=b(n[3][5],s,g);if(t)if(u){var
c=u[1],d=t[1],v=a(e[25],c);if(-1===el(a(e[25],d),v)){var
w=a(e[15],c),x=b(e[9],o,w),y=a(e[15],d),z=b(e[9],p,y),A=b(e[1],z,x),B=c3(h,f),C=[0,g,a(e[15],c)],D=[0,bB([0,i,a(e[15],d)],C),B,A],E=[0,k,a(e[15],c)];return[0,[0,bB([0,m,a(e[15],d)],E),D]]}if(0===h){var
F=b(e[9],o,mr),G=b(e[9],d,c),H=a(e[3],G),I=b(e[9],p,H),J=b(e[1],I,F),K=c3(h,f),L=b(e[9],d,c),M=[0,bB([0,i,a(e[3],L)],[0,g,ms]),K,J],N=b(e[9],d,c);return[0,[0,bB([0,m,a(e[3],N)],[0,k,mt]),M]]}if(0===f){var
O=b(e[9],p,mu),P=b(e[9],c,d),Q=a(e[3],P),R=b(e[9],o,Q),S=b(e[1],R,O),T=c3(h,f),U=b(e[9],c,d),V=[0,bB([0,g,a(e[3],U)],[0,i,mv]),T,S],W=b(e[9],c,d);return[0,[0,bB([0,k,a(e[3],W)],[0,m,mw]),V]]}return 0}return 0}var
mD=[0,function(y,c){function
d(c){switch(c[0]){case
0:var
j=c[1];return[0,[0,[0,[0,j,mB],0],b(m[17][7],y,j)],0];case
1:var
z=c[3],A=c[1],B=d(c[2]),C=d(z),v=0,w=function(a,c){function
b(a,d){var
b=gW(A,c,d);return b?[0,b[1],a]:a}return f(m[17][15],b,a,C)};return f(m[17][15],w,v,B);default:var
D=c[2],E=d(c[1]),F=d(D),G=b(m[18],E,F),x=function(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=gO(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b},h=f(m[17][15],x,mx,G);if(0===h[0]){var
H=h[1],I=function(j,i){if(0===j[0]){var
t=j[1],k=i[2],m=i[1],n=i[4][1],x=t[2],y=t[1],u=n[2],v=n[1],o=function(e,c,a){if(c){var
f=c[1][3];if(a){var
d=a[1];return b(e,f,d)?[0,[0,m,k,d]]:c}return c}return a?[0,[0,m,k,a[1]]]:0},c=o(e[29],y,v),d=o(e[30],x,u);if(c)if(d){var
f=d[1],g=f[2],p=f[1],h=c[1],q=h[1],w=h[2];if(b(e[29],h[3],f[3]))return[0,[0,c,d]];var
r=g[1];if(r){var
s=gW(r[1][1],[0,q,w],[0,p,g]);return s?[1,s[1]]:a(l[3],my)}return[1,[0,bB([0,q,mA],[0,p,mz]),g]]}return[0,[0,c,d]]}return j},i=f(m[17][15],I,mC,H);if(0===i[0]){var
k=i[1],g=k[2],n=k[1];if(n){var
o=n[1],p=o[2],q=o[1];if(g){var
r=g[1];return[0,[0,q,p],[0,[0,r[1],r[2]],0]]}var
t=p,s=q}else{if(!g)return 0;var
u=g[1],t=u[2],s=u[1]}return[0,[0,s,t],0]}return[0,i[1],0]}return[0,h[1],0]}}return d(c)},c3],Q=[0,[0,e4,lL],[0,A[1],A[2],A[3],A[4],A[5],A[6],A[7],A[8],A[9],A[10],A[11],A[12],A[13],A[14],A[15],A[16],A[17],A[18],A[19],A[20],A[21],A[22],A[23],A[24]],[0,mq,mm],cg,mD,gL,lW,gM];aJ(616,Q,"Micromega_plugin.Mfourier");var
mE=0,mF=p[13],mG=p[12][8],mI=0;function
mJ(b){return[1,a(k[8][1],b)]}var
e8=[0,k[7][2],mJ,mI,mH,mG,mF],mK=p[77],mL=p[80],mO=k[8][2],a8=[0,function(b){return[0,a(k[7][2],b),0]},mO,mN,mM,mL,mK];function
c4(e,c){function
d(c){switch(c[0]){case
0:var
g=a(e[2],c[1]);return a(n[2][1],g);case
1:var
h=a(k[8][3],c[1]);return a(n[2][2],h);case
2:var
i=c[1],j=d(c[2]),l=d(i);return b(n[2][3],l,j);case
3:var
m=c[1],o=d(c[2]),p=a(n[2][5],o),q=d(m);return b(n[2][3],q,p);case
4:var
r=c[2],s=d(c[1]),t=d(r);return b(n[2][4],s,t);case
5:var
u=d(c[1]);return a(n[2][5],u);default:var
v=c[2],w=d(c[1]),x=a(k[8][4],v),f=function(c){if(0===c){var
d=a(e[2],e[4]);return a(n[2][1],d)}var
g=f(c-1|0);return b(n[2][4],w,g)};return f(x)}}return d(c)}function
e9(b,f){var
c=f;for(;;){var
e=aa(p[74],b[3],b[4],b[5],b[6]),d=function(d){function
c(b){if(typeof
b!=="number")switch(b[0]){case
3:var
e=b[1],f=c(b[2]);return a(d,[3,c(e),f]);case
4:var
g=b[1],h=c(b[2]);return a(d,[4,c(g),h])}return a(d,b)}return c}(e)(c);if(aj(d,c))return d;var
c=d;continue}}var
e_=a(m[20][1],[0,n[1][6]]);function
mR(c){function
g(a){return a[1]}var
e=b(m[17][69],g,c),h=a(e_[5],n[1][2]);function
i(c,a){function
d(c,d,a){return b(e_[4],c,a)}return f(n[2][7],d,a,c)}var
j=f(m[17][15],i,h,e),k=0;function
l(d,q){var
h=0;function
i(c,a){return[0,b(n[2][6],d,a),c]}var
c=f(m[17][15],i,h,e);if(aj(d,n[1][2]))var
j=[1,o[1]],k=a(m[17][9],c),g=[0,a(n[3][10],[0,[1,o[2]],k]),0,j];else
var
l=[1,o[1]],p=a(m[17][9],c),g=[0,a(n[3][10],[0,[1,o[1]],p]),0,l];return[0,g,q]}var
p=f(e_[15],l,j,k),q=[1,o[2]],r=1;function
s(a){return 2===a[2]?[1,o[2]]:[1,o[1]]}var
t=b(m[17][69],s,c),u=[0,a(n[3][10],[0,[1,o[2]],t]),r,q];function
d(e,c){var
b=e,a=c;for(;;){if(a){if(0===a[1][2]){var
b=b+1|0,a=a[2];continue}var
g=d(b+1|0,a[2]),h=1,i=n[3][9],j=function(a){return mQ};return[0,[0,f(n[3][8],b+1|0,j,i),h,mP],g]}return 0}}var
v=[0,u,d(0,c)],w=b(m[18],v,p),x=[1,o[1]];return[0,[0,a(n[3][10],[0,[1,o[2]],0]),1,x],w]}function
gX(f,e){var
g=e[1],h=f[1],q=e[2];if(g){var
r=g[2],i=function(d,c){if(d){var
e=d[1],m=d[2];if(c){var
g=c[1],f=i(m,c[2]),j=b(o[23],e,o[1]);if(-1===j){var
n=[0,a(k[7][4],g)];return[4,[2,[0,a(h,e)],n],f]}if(0===j)return f;var
p=[0,a(k[7][4],g)];return[4,[3,[5,a(h,e)],p],f]}return a(l[3],mS)}return 0},s=e9(f,i(r,q)),j=function(p,o){var
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
h=c[1],k=a[2],l=c[2];if(aj(h,a[1]))var
e=[0,[2,h,[4,l,k]]],b=1;else
var
e=0,b=1}else
var
b=0;break;case
3:if(typeof
a==="number")var
b=0;else
if(3===a[0]){var
i=c[1],m=a[2],n=c[2];if(aj(i,a[1]))var
e=[0,[3,i,[4,n,m]]],b=1;else
var
e=0,b=1}else
var
b=0;break;default:var
b=0}if(!b)var
e=0;if(e){var
f=g,d=[0,e[1]];continue}return[4,c,j(g,[0,a])]}var
f=g,d=[0,a];continue}return d?d[1]:0}},c=s,d=0,p=0;for(;;){if(typeof
c!=="number"&&4===c[0]){var
n=[0,c[1],d],c=c[2],d=n;continue}return j(b(m[17][39],D.caml_compare,[0,c,d]),p)}}return a(l[3],mT)}var
gY=[bs,mU,bq(0)],aP=a(m[21][1],[0,n[1][6]]);function
mV(c){var
d=[0,0];function
p(b,a){return ab(b[1],a[1])}var
g=[0,aP[1],0];function
h(h,g){var
i=h[2],j=h[1],q=g[2],l=[0,g[1],0];function
o(g,j,i){var
h=i[2],c=i[1];if(aj(g,n[1][2]))return[0,c,h];try{var
q=b(aP[22],g,c),l=q,k=c}catch(a){a=w(a);if(a!==J)throw a;var
m=f(aP[4],g,d[1],c),o=d[1];d[1]++;var
l=o,k=m}var
p=0===a(e[25],j)?h:[0,[0,l,j],h];return[0,k,p]}var
c=f(n[2][7],o,j,l),r=c[2],s=c[1],t=b(n[2][6],n[1][2],j),u=a(e[3],t);if(0===i)var
k=0;else{if(!(3<=i))throw gY;var
k=1}return[0,s,[0,[0,b(m[17][39],p,r),k,u],q]]}return f(m[17][16],h,c,g)[2]}function
mW(e){try{var
f=mV(e),g=a(Q[3][1],f);if(0===g[0])var
h=0;else
var
l=b(Q[5][1],f,g[1]),o=a(m[17][5],l)[1],p=function(a){return[0,a[1]+1|0,a[2]]},q=b(m[17][69],p,o),r=a(n[3][11],q),h=[0,a(k[9],r)];return h}catch(b){b=w(b);if(b===gY){var
i=mR(e);try{var
c=a(Q[3][1],i);if(0===c[0])var
j=a(n[3][11],c[1]),d=[0,a(k[9],j)];else
var
d=0;return d}catch(b){b=w(b);if(a(a7[18],b))return 0;throw b}}throw b}}function
mX(g,e){var
h=a(m[17][1],e)-1|0,i=b(mY[53],0,h),j=b(m[17][124],e,i);function
k(a){return 1===a[1][2]?1:0}var
n=b(m[17][30],k,j)[2];function
o(b){var
c=b[1],d=c[2],e=b[2],f=c[1];return 1===d?a(l[3],mZ):[0,[0,c4(g,f),d],e]}var
p=b(m[17][69],o,n),c=a(m[17][es],p),f=c[2],d=mW(c[1]);return d?[0,[0,d[1],f]]:0}function
gZ(c,b){try{var
e=mX(c,b);return e}catch(b){b=w(b);if(a(a7[18],b)){var
d=a(eT[1],b);a(l[31],d);return 0}throw b}}function
c5(e,c){var
d=a(m[17][1],e),f=b(l[6],c,el(d,c));return b(l[6],d,f)}function
m0(d,b,a){var
e=c5(a,d);Q[6][1]=e;var
c=gZ(b,a);return c?[0,gX(b,c[1])]:0}function
m1(l,c){a(n[9][1][1],0);var
o=c5(c,l);Q[6][1]=o;function
q(c,b){return[0,b,[0,a(k[7][4],c)]]}var
e=b(m[17][13],q,c);function
r(c,a){var
d=a[1],e=c[1],f=d[1],g=e[1],h=[3,c[2],a[2]];return[0,[0,[4,g,f],b(p[53],e[2],d[2])],h]}var
s=b(k[10],r,e),t=0;function
u(b,a){var
c=a[1],d=c[2],e=a[2],f=c[1];return d?[0,[0,[0,f,d[1]],e],b]:b}var
v=f(m[17][15],u,t,s),g=b(m[18],e,v);function
w(a){return c4(a8,a[1][1])}var
x=b(m[17][69],w,g),y=aP[1];function
z(c,b){function
d(c,e,b){var
d=a(n[1][3],c);return d?f(aP[4],d[1],c,b):b}return f(n[2][7],d,b,c)}var
A=f(m[17][15],z,y,x);function
h(b){var
c=[0,a8[4]];function
d(d,c,b){var
e=a(k[7][3],c);return[4,[6,[1,a(k[7][1],d)],e],b]}return f(n[1][1],d,b,c)}var
B=fw(p[63],a8[3],a8[4],p[79],p[80],p[82],p[81],p[77]),C=0;function
D(d,c,b){var
e=[1,a(B,h(d))];return[0,[0,[0,h(c),3],e],b]}var
E=f(aP[11],D,A,C),i=b(m[18],g,E);function
F(a){return a[1]}var
j=gZ(a8,b(m[17][69],F,i));if(j){var
G=gX(a8,j[1]),d=function(c){if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
e=a(k[8][5],c[1]);return b(m[17][7],i,e)[2];case
1:return[1,c[1]];case
2:var
f=c[1];return[2,f,d(c[2])];case
3:var
g=c[1],h=d(c[2]);return[3,d(g),h];case
4:var
j=c[1],l=d(c[2]);return[4,d(j),l];default:return[5,c[1]]}};return[0,d(G)]}return 0}function
g0(b,a){var
c=a[2],d=a[1];if(0===c)return[0,c4(b,d),0];if(3<=c)return[0,c4(b,d),1];throw[0,ay,m2]}function
bC(c){if(typeof
c==="number")return[0,o[2],0];else
switch(c[0]){case
0:var
e=c[1],z=[0,[1,a(k[1],e)]];return[0,a(k[2],e),z];case
1:return[0,o[2],[1,c[1]]];case
2:return a(l[3],m3);case
3:var
f=bC(c[1]);return[0,f[1],[3,f[2]]];case
4:var
g=c[1],A=g[2],h=bC(g[1]),i=h[2],j=h[1],m=bC(A),n=m[2],p=m[1],d=b(o[17],j,p),q=b(o[15],j,d),r=b(o[15],p,d),B=b(o[10],q,r),s=b(o[10],d,B);return 0===b(o[23],s,o[2])?[0,o[2],[4,[0,i,n]]]:[0,s,[4,[0,[6,[0,[0,[1,r]],i]],[6,[0,[0,[1,q]],n]]]]];case
5:return a(l[3],m4);case
6:var
t=c[1],C=t[2],u=bC(t[1]),D=u[2],E=u[1],v=bC(C),F=[6,[0,D,v[2]]];return[0,b(o[10],E,v[1]),F];case
7:return a(l[3],m5);default:var
w=c[1],x=w[2],y=bC(w[1]),G=[8,[0,y[2],x]];return[0,b(o[19],y[1],x),G]}}function
g1(b){var
a=bC(b);return[0,a[1],a[2]]}function
bQ(c){switch(c[0]){case
0:return[0,o[2],[0,c[1]]];case
1:return[0,o[2],[1,c[1]]];case
2:return[0,o[2],[2,c[1]]];case
3:var
e=c[1],v=[3,[1,a(k[1],e)]];return[0,a(k[2],e),v];case
4:var
f=c[1],w=[4,[1,a(k[1],f)]];return[0,a(k[2],f),w];case
5:var
g=c[1],x=[5,[1,a(k[1],g)]];return[0,a(k[2],g),x];case
6:var
h=g1(c[1]),i=h[1],y=[6,h[2]];return[0,b(o[10],i,i),y];case
7:return[0,o[2],[7,c[1]]];case
8:var
z=c[2],j=g1(c[1]),A=j[2],B=j[1],l=bQ(z),C=[8,A,l[2]];return[0,b(o[10],B,l[1]),C];case
9:var
D=c[2],m=bQ(c[1]),n=m[1],E=m[2],p=bQ(D),q=p[1],F=p[2],d=b(o[17],n,q),r=b(o[15],n,d),s=b(o[15],q,d),G=b(o[10],r,s);return[0,b(o[10],d,G),[9,[10,[4,[1,s]],E],[10,[4,[1,r]],F]]];default:var
H=c[2],t=bQ(c[1]),I=t[2],J=t[1],u=bQ(H),K=[10,I,u[2]];return[0,b(o[10],J,u[1]),K]}}function
aQ(b){if(typeof
b==="number")return[0,a(k[7][5],m6)];else
switch(b[0]){case
0:return[0,a(k[7][5],b[1])];case
1:var
c=b[1],i=hP(f(m[15][4],c,1,hO(c)-1|0));return[1,a(k[7][6],i)];case
3:return[5,aQ(b[1])];case
4:var
d=b[1],j=d[1],n=aQ(d[2]);return[2,aQ(j),n];case
5:var
e=b[1],o=e[1],p=aQ(e[2]);return[3,aQ(o),p];case
6:var
g=b[1],q=g[1],r=aQ(g[2]);return[4,aQ(q),r];case
8:var
h=b[1],s=h[1],t=a(k[7][3],h[2]);return[6,aQ(s),t];default:return a(l[3],m7)}}function
g2(b){var
c=aQ(b),d=p[77],e=p[81],f=p[82],g=p[80],h=p[79],i=a(k[7][5],m8),j=a(k[7][5],m9);return em(p[39],j,i,h,g,f,e,d,c)}function
e$(b){if(b){var
c=b[2],d=b[1];if(c){var
e=e$(c);return[3,[0,a(k[7][4],d)],e]}return[0,a(k[7][4],d)]}return 0}function
m_(c){function
d(c){switch(c[0]){case
0:return[0,a(k[7][4],c[1])];case
1:return[0,a(k[7][4],c[1])];case
2:return[0,a(k[7][4],c[1])];case
6:return[1,g2(c[1])];case
7:return e$(c[1]);case
8:var
g=c[1],h=d(c[2]);return[2,g2(g),h];case
9:var
i=c[1],j=d(c[2]);return[4,d(i),j];case
10:var
l=c[1],m=d(c[2]);return[3,d(l),m];default:var
f=c[1];return 0===b(e[37],f,m$)?0:[5,a(k[7][5],f)]}}return e9(a8,d(c))}function
aR(b){if(typeof
b==="number")return na;else
switch(b[0]){case
0:var
j=a(e[52],b[1]);return[0,a(k[7][2],j)];case
1:var
c=b[1],n=hP(f(m[15][4],c,1,hO(c)-1|0));return[1,a(k[7][6],n)];case
3:return[5,aR(b[1])];case
4:var
d=b[1],o=d[1],p=aR(d[2]);return[2,aR(o),p];case
5:var
g=b[1],q=g[1],r=aR(g[2]);return[3,aR(q),r];case
6:var
h=b[1],s=h[1],t=aR(h[2]);return[4,aR(s),t];case
8:var
i=b[1],u=i[1],v=a(k[7][3],i[2]);return[6,aR(u),v];default:return a(l[3],nb)}}function
g3(b){var
c=aR(b),d=p[13],e=p[12][6],f=p[12][7],g=p[12][8],h=p[12][5],i=a(k[7][7],1),j=a(k[7][7],0);return em(p[39],j,i,h,g,f,e,d,c)}function
nc(c){var
f=bQ(c)[2];function
d(l){var
c=l;for(;;)switch(c[0]){case
0:return[0,a(k[7][4],c[1])];case
1:return[0,a(k[7][4],c[1])];case
2:return[0,a(k[7][4],c[1])];case
6:return[1,g3(c[1])];case
7:return e$(c[1]);case
8:var
i=c[2],f=c[1];if(typeof
f==="number")var
g=0;else
if(0===f[0])var
j=b(e[26],f[1],ne),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
c=i;continue}var
n=d(i);return[2,g3(f),n];case
9:var
o=c[1],p=d(c[2]);return[4,d(o),p];case
10:var
q=c[1],r=d(c[2]);return[3,d(q),r];default:var
h=c[1];if(0===b(e[37],h,nd))return 0;var
m=a(e[52],h);return[5,a(k[7][2],m)]}}return e9(e8,d(f))}function
g4(b){var
c=b[1],d=a(k[1],b[2]),e=[0,a(k[7][2],d)];function
g(e,b){var
g=b[2],h=a(n[9][1][2],b[1]);function
c(d,c,b){var
e=a(k[7][3],c);return[4,[6,[1,a(k[7][1],d)],e],b]}var
d=f(n[1][1],c,h,ng),i=a(k[1],g);return[2,[4,[0,a(k[7][2],i)],d],e]}return f(m[17][15],g,e,c)}function
aS(c,b){if(typeof
b==="number")return 0;else
switch(b[0]){case
2:return[5,a(k[7][2],b[1])];case
3:var
i=g4(b[1]);return[1,a(p[92],i)];case
4:var
j=b[2],m=g4(b[1]),n=a(p[92],m);return[2,n,aS(c,j)];case
6:var
o=b[1],q=aS(c,b[2]);return[3,aS(c,o),q];case
7:var
r=b[1],s=aS(c,b[2]);return[4,aS(c,r),s];case
0:case
1:var
e=0,d=c,h=b[1];for(;;){if(d){var
g=d[2];if(!aj(h,d[1])){var
e=e+1|0,d=g;continue}var
f=e}else
var
f=a(l[3],nf);return[0,a(k[7][4],f)]}default:return a(l[3],nh)}}function
c6(c,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
e=a[3],d=a[2],f=a[1];if(typeof
d!=="number"&&8===d[0]){var
h=d[1],i=c6([0,f,c],e);return[1,aS(c,h),i]}var
g=c6([0,f,c],e);return[0,aS(c,d),g]}var
j=a[5],k=a[4],l=a[2],n=[0,a[1],c],o=function(a){return c6(n,a)},p=b(m[17][69],o,j),q=aS(c,k);return[2,aS(c,l),q,p]}}function
ni(d,c){var
e=1+a(n[4],c)|0;return c6(d,b(n[5],e,c)[2])}function
fa(c){var
d=a(Q[3][1],c);if(0===d[0])return 0;var
e=b(Q[5][1],c,d[1]),f=a(m[17][5],e)[1],g=a(n[3][11],f);return[0,a(k[9],g)]}function
fb(g,f){var
e=0,d=g,c=f;for(;;){if(c){if(d){var
h=c[2],i=d[2],j=b(n[8],c[1],d[1]),e=b(n[7],j,e),d=i,c=h;continue}return a(l[3],nj)}return e}}function
nk(d){var
b=a(m[17][es],d),e=b[2],c=fa(b[1]);return c?[0,fb(e,c[1])]:0}function
g5(l){var
d=l[2],g=l[1],h=g[3],i=g[2],j=g[1];if(j){var
o=function(a){return a[2]},p=b(m[17][69],o,j),n=a(k[14],p),c=[1,n];if(b(e[32],c,nl))return[2,g,d];var
q=b(e[12],h,c);if(0===a(e[25],q)){if(1<=a(e[25],c)){var
r=b(e[9],h,c),s=function(a){var
d=a[1];return[0,d,b(e[9],a[2],c)]};return[2,[0,b(m[17][69],s,j),i,r],[5,n,d]]}throw[0,ay,nm]}if(0===i)return[0,[8,d]];var
t=b(e[9],h,c),u=a(e[24],t),v=function(a){var
d=a[1];return[0,d,b(e[9],a[2],c)]};return[1,[0,b(m[17][69],v,j),i,u],[8,d]]}return f(Q[7],i,nn,h)?0:[0,d]}var
c7=[bs,no,bq(0)];function
np(a){var
b=0;function
c(b,c){var
a=g5([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,c7,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return f(m[17][15],c,b,a)}function
fc(g,c){if(0===a(o[22],c))return[0,o[2],o[1]];var
d=b(o[14],g,c),h=d[1],e=fc(c,d[2]),f=e[2],i=e[1],j=b(o[10],h,f);return[0,f,b(o[8],i,j)]}function
fd(g,b){var
c=0;function
d(c,d){var
e=a(g,d);if(e){var
b=g5(e[1]);if(typeof
b==="number")return c;else
switch(b[0]){case
0:throw[0,c7,b[1]];case
1:return[0,[0,b[1],b[2]],c];default:return[0,[0,b[1],b[2]],c]}}return[0,d,c]}return f(m[17][15],d,c,b)}function
g6(r,q,c){return fd(function(s){var
f=s[1],g=q[1],i=f[2],j=f[1],l=g[2],m=g[1],t=s[2],u=q[2],v=f[3],w=g[3];function
h(d,c){var
f=a(k[1],c),g=b(n[8],f,t),h=a(k[1],d),o=[7,b(n[8],h,u),g],p=b(e[6],v,c),q=b(e[6],w,d),r=b(e[1],q,p),s=b(Q[5][2],l,i),x=b(n[3][13],c,j),y=b(n[3][13],d,m);return[0,[0,b(n[3][12],y,x),s,r],o]}var
o=b(n[3][5],r,m),p=b(n[3][5],r,j);if(o)if(p){var
c=p[1],d=o[1],x=a(e[25],c);if(-1===el(a(e[25],d),x)){var
y=a(e[15],c);return[0,h(y,a(e[15],d))]}if(0===l){var
z=[0,a(e[25],d)],A=b(e[6],c,z),B=a(e[3],A);return[0,h(B,a(e[15],d))]}if(0===i){var
C=a(e[15],c),D=[0,a(e[25],c)],E=b(e[6],d,D);return[0,h(C,a(e[3],E))]}return 0}return 0},c)}function
nq(D){var
d=0,c=D;for(;;){if(c){var
q=c[2],f=c[1],A=function(v){return function(u){var
f=u[1],g=v[1];if(0===g[2])if(0===f[2]){var
d=g[1],c=f[1];for(;;){if(d)if(c){var
h=c[2],i=c[1],j=i[2],l=i[1],m=d[2],n=d[1],p=n[2],e=n[1];if(aj(e,l)){var
q=o[2],r=a(k[1],j),s=a(k[1],p),t=b(o[17],s,r);if(0===b(o[23],t,q))return[0,[0,e,p,j]];var
d=m,c=h;continue}if(fv(e,l)){var
d=m;continue}var
c=h;continue}return 0}}return 0}}(f),r=b(k[15],A,q),s=r[1];if(!s){var
d=[0,f,d],c=q;continue}var
t=s[1],B=t[2],C=t[1],g=[0,[0,[0,C,f,B]],b(m[17][10],d,r[2])]}else
var
g=[0,0,d];var
u=g[1],E=g[2];if(u){var
h=u[1],v=h[3],w=v[1],x=h[2],y=x[2],i=x[1],j=h[1],F=v[2],G=j[2],H=j[1],I=a(k[1],j[3]),z=fc(a(k[1],G),I),l=[1,z[1]],p=[1,z[2]],J=b(e[6],p,w[3]),K=b(e[6],l,i[3]),L=b(e[1],K,J),M=b(n[3][13],p,w[1]),N=b(n[3][13],l,i[1]),O=[0,b(n[3][12],N,M),0,L],P=a(k[1],p),Q=b(n[8],P,F),R=a(k[1],l),S=b(n[8],R,y);return[0,g6(H,[0,O,b(n[7],S,Q)],[0,[0,i,y],E])]}return 0}}function
nr(f){function
g(c){var
a=c[1];if(0===a[2])try{var
d=a[1],f=function(d){var
a=d[2],c=b(e[26],a,ns);return c?c:b(e[26],a,nt)},g=[0,b(m[17][27],f,d)[1]];return g}catch(a){a=w(a);if(a===J)return 0;throw a}return 0}var
a=b(k[15],g,f),c=a[1],h=a[2];if(c){var
d=c[1];return[0,g6(d[1],d[2],h)]}return 0}function
nu(g){function
h(f){var
c=f[1];if(0===c[2])try{var
g=c[1],h=function(c){var
d=c[2],g=c[1],h=b(e[26],d,nv),f=h||b(e[26],d,nw);if(f){var
i=a(n[9][1][2],g);return a(n[1][4],i)}return f},d=b(m[17][27],h,g)[1],i=a(n[9][1][2],d),j=c[1],k=function(g){var
c=g[1],e=c===d?1:0;if(e)var
f=e;else
var
h=a(n[9][1][2],c),f=0===b(n[1][5],h,i)[2]?1:0;return f},l=b(m[17][21],k,j)?[0,d]:0;return l}catch(a){a=w(a);if(a===J)return 0;throw a}return 0}var
c=b(k[15],h,g),d=c[1],i=c[2];if(d){var
f=d[1];return[0,fd(b(n[9][2],f[1],f[2]),i)]}return 0}function
nx(r){function
s(h){var
c=h;for(;;){if(c){var
d=c[2],e=c[1],i=e[1],f=a(k[1],e[2]);try{var
j=function(g){return function(c){var
d=a(k[1],c[2]),e=o[2],f=b(o[17],g,d);return b(o[24],f,e)}}(f),g=b(m[17][27],j,d),l=g[1],n=[0,[0,[0,i,f],[0,l,a(k[1],g[2])]]];return n}catch(a){a=w(a);if(a===J){var
c=d;continue}throw a}}return 0}}function
t(b){var
a=b[1];return 0===a[2]?s(a[1]):0}var
c=b(k[15],t,r),d=c[1],u=c[2];if(d){var
f=d[1],g=f[2],h=g[1],i=f[1],j=i[2],l=i[1],v=g[2],x=j[1],y=l[1],p=fc(l[2],j[2]),z=[1,p[2]],A=[1,p[1]],q=function(d,c){var
a=b(n[3][5],d,c);return a?a[1]:ny};return[0,fd(function(g){var
c=g[1],d=c[1],i=g[2],j=c[3],k=c[2],l=q(y,d),m=q(x,d),o=b(e[6],m,z),p=b(e[6],l,A),r=b(e[1],p,o),f=a(e[3],r),s=b(e[6],f,h[3]),t=b(e[1],s,j),u=b(n[3][13],f,h[1]);return[0,[0,[0,b(n[3][12],u,d),k,t],[7,[4,[0,0,f],v],i]]]},u)]}return 0}function
g7(e,d){var
b=d;for(;;){var
c=a(e,b);if(c){var
b=c[1];continue}return b}}function
g8(e,d){var
b=e;for(;;){if(b){var
f=b[2],c=a(b[1],d);if(c)return[0,c[1]];var
b=f;continue}return 0}}function
g9(a){var
b=[0,nr,[0,nq,[0,nx,0]]];return g7(function(a){return g8(b,a)},a)}function
nz(a){var
b=[0,nu,0];return g7(function(a){return g8(b,a)},a)}function
fe(c){function
d(c){var
d=c[1][1];function
f(b){return 0!==a(e[25],b[2])?1:0}return b(m[17][21],f,d)}return b(m[17][21],d,c)}function
g_(j,i,c){function
p(G,q){if(fe(q)){var
H=a(m[17][es],q),I=H[2],d=H[1],L=function(a){return 0===a[2]?1:0},v=b(m[17][30],L,d),w=v[2],x=v[1];if(x)var
M=0,N=function(c,a){function
d(c){return b(n[3][2],a[1],c[1])}return b(m[17][22],d,x)?c:[0,a[1],c]},y=f(m[17][15],N,M,w);else
var
O=function(a){return a[1]},y=b(m[17][14],O,w);var
P=[0,n[3][9],nB],R=function(c,h){var
f=a(Q[1][1],c[2]),l=f?b(e[29],f[1],nA):0;if(l)return c;var
i=b(Q[3][2],h,d);if(i){var
j=i[1],g=c[2],k=c[1];return b(Q[1][2],j,g)?[0,h,j]:[0,k,g]}return c},z=f(m[17][15],R,P,y),A=z[2],B=A[1],S=z[1];if(B){var
C=A[2];if(C)var
g=[0,[0,B[1],S,C[1]]],t=1;else
var
t=0}else
var
t=0;if(!t)var
g=0;if(g){var
h=g[1],i=h[3],j=h[2],p=h[1],T=a(k[2],p),U=o[2],V=a(k[1],p),W=b(o[8],V,U),X=a(k[2],i),Y=a(k[1],i),Z=[1,b(o[5],o[2],Y)],D=fa([0,[0,b(n[3][13],[1,X],j),1,Z],d]),_=a(e[3],[1,W]),$=a(e[3],[1,T]),E=fa([0,[0,b(n[3][13],$,j),1,_],d]);if(D)if(E)var
aa=E[1],ab=a(m[17][6],D[1]),c=[0,[0,a(m[17][6],aa),[0,p,j,i],ab]],u=1;else
var
u=0;else
var
u=0;if(!u)var
c=a(l[3],nC)}else
var
c=0;if(c){var
r=c[1],s=r[2],J=s[2],ac=r[3],ad=s[1],ae=r[1],af=a(e[22],s[3]),K=F(G,J,a(e[24],ad),af,q);if(K){var
ag=K[1],ah=fb(I,ac);return[0,[1,G,fb(I,ae),J,ah,ag]]}return 0}return 0}throw[0,ay,nD]}function
F(c,h,a,f,d){if(b(e[28],a,f))return nE;var
i=g(c+1|0,[0,[0,[0,h,0,a],[1,c]],d]);if(i){var
k=i[1],j=F(c,h,b(e[1],a,nF),f,d);return j?[0,[0,k,j[1]]]:0}return 0}function
g(b,c){if(fe(c))try{var
d=a(i,c),e=nk(d),f=e?[0,[0,b,e[1],0]]:j?p(b,d):0;return f}catch(a){a=w(a);if(a[1]===c7)return[0,[0,b,a[2],0]];throw a}throw[0,ay,nG]}var
h=a(m[17][1],c);try{var
s=g(h,np(c)),d=s}catch(a){a=w(a);if(a[1]!==c7)throw a;var
d=[0,[0,h,a[2],0]]}if(d){var
q=d[1],r=function(a,b){return a};return[0,ni(b(m[17][13],r,c),q)]}return 0}function
g$(b){var
d=b[2],c=a(n[9][3],b[1]),f=c[1];return[0,f,d,a(e[3],c[2])]}function
nH(e,d,c){a(n[9][1][1],0);var
f=c5(c,d);Q[6][1]=f;function
g(a){return g0(e8,a)}var
h=b(m[17][69],g,c),i=b(m[17][69],g$,h);function
j(b,a){return[0,a,[0,b]]}return g_(e,g9,b(m[17][13],j,i))}var
az=[0,mE,m_,nc,nH,function(l,j,e){a(n[9][1][1],0);var
o=c5(e,j);Q[6][1]=o;function
p(a){return g0(e8,a)}var
q=b(m[17][69],p,e);function
r(b,a){return[0,a,[0,b]]}var
c=b(m[17][13],r,q);function
s(b){return a(n[2][8],b[1][1])}var
g=b(m[17][21],s,c),t=aP[1];function
u(c,b){var
d=b[1][1];function
e(c,e,b){var
d=a(n[1][3],c);return d?f(aP[4],d[1],c,b):b}return f(n[2][7],e,d,c)}var
v=f(m[17][15],u,t,c);function
w(d,c,b){var
e=a(n[2][1],nI),g=f(n[2][9],d,nJ,e),h=a(n[9][3],g),i=a(n[2][1],nK);return[0,[0,[0,f(n[2][9],c,nL,i),1],[3,h]],b]}var
d=f(aP[11],w,v,c);if(g)var
h=d;else
var
z=function(c,a){var
d=a[1],e=c[1],f=d[1],g=e[1],h=[6,c[2],a[2]],i=b(n[11],e[2],d[2]);return[0,[0,b(n[2][4],g,f),i],h]},A=b(k[11],z,d),h=b(m[18],d,A);function
x(a){var
b=a[2];return[0,g$(a[1]),b]}var
i=b(m[17][69],x,h);if(fe(i)){var
y=g?g9:nz;return g_(l,y,i)}throw[0,ay,nM]},m1,m0,a8];aJ(618,az,"Micromega_plugin.Certificate");var
ch=[0,function(p){var
c=a(bA[25],p),k=[bs,nN,bq(0)],e=[bs,nO,bq(0)];function
q(d,b){var
e=a(c[1],d),g=f(C[23],b,nP,ep);return[0,a(C[31],g),1,e]}function
r(b,c){try{var
d=a(b,0);a(c,0);return d}catch(b){b=w(b);try{a(c,0)}catch(a){throw b}throw b}}function
s(b){try{var
c=[0,a(cT[3],b)];return c}catch(b){b=w(b);if(b===nQ)return 0;if(a(a7[18],b))throw k;throw b}}function
t(c,a){var
d=f(C[34],a,0,1);try{f(C[34],a,0,0);var
e=0===c?4:1;f(C[83],a,e,1);var
g=1,b=g}catch(a){a=w(a);if(a[1]!==C[1])throw a;var
b=0}f(C[34],a,d,0);return b}function
u(a){var
c=f(C[34],a,0,1);try{f(C[34],a,0,0);var
b=f(C[83],a,0,1);return b}catch(b){b=w(b);if(b[1]===C[1]){f(C[34],a,c,0);return 0}throw b}}function
j(d,c,b){return t(d,c)?r(b,function(a){return u(c)}):a(b,0)}function
m(g){var
e=f(C[23],g,nR,ep),h=a(C[30],e),d=a(c[1],100);function
n(e){for(;;){var
a=s(h);if(a){var
b=a[1];f(c[10],d,b[1],b[2]);continue}return 0}}try{j(0,e,n);a(l[83],h);var
o=f(C[23],g,nU,ep),p=[0,a(C[31],o),1,d];return p}catch(e){e=w(e);if(e===k){a(l[83],h);var
m=f(C[23],g,nS,ep),i=a(C[31],m);j(1,m,function(g){function
e(b,a){return f(cT[1],i,[0,b,a],nT)}b(c[12],e,d);return a(l[52],i)});return[0,i,1,d]}throw e}}function
v(b){var
d=b[1],e=b[3];return 0===b[2]?0:(a(l[65],d),a(c[2],e),b[2]=0,0)}function
n(b,h,g){var
d=b[1],i=b[3];if(0===b[2])throw e;var
k=a(C[33],d);f(c[10],i,h,g);return j(1,k,function(b){f(cT[1],d,[0,h,g],nV);return a(l[52],d)})}function
o(a,d){var
f=a[3];if(0===a[2])throw e;return b(c[7],f,d)}return[0,q,m,o,n,v,function(c,e){var
b=[d,function(b){try{var
a=[0,m(c)];return a}catch(a){return 0}}];return function(c){var
f=h(b),j=i===f?b[1]:d===f?a(g[2],b):b;if(j){var
k=j[1];try{var
m=o(k,c);return m}catch(b){b=w(b);if(b===J){var
l=a(e,c);n(k,c,l);return l}throw b}}return a(e,c)}}]}];aJ(621,ch,"Micromega_plugin.Persistent_cache");var
ff=l[8],ha=[0,ff],fg=[0,1],hb=[0,ff];function
hc(a){return[0,fg[1],hb[1]]}function
fh(a){return ha[1]}function
hd(b,a){function
c(b){var
c=b?b[1]:ff;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,0,f(q[21],l[17],b,nW),b,d,c]}function
nX(a){fg[1]=a;return 0}var
n0=[0,0,nZ,nY,function(a){return fg[1]},nX],n2=hd(n1,ha);b(fi[3],0,n2);var
n4=hd(n3,hb);b(fi[3],0,n4);b(fi[4],0,n0);function
aT(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,b[1]];case
1:var
d=b[3],e=b[2];return[1,a(c,b[1]),e,d];case
2:var
f=b[1],g=aT(c,b[2]);return[2,aT(c,f),g];case
3:var
h=b[1],i=aT(c,b[2]);return[3,aT(c,h),i];case
4:return[4,aT(c,b[1])];default:var
j=b[2],k=b[1],l=aT(c,b[3]);return[5,aT(c,k),j,l]}}function
a_(c,b){if(typeof
b==="number")return 0===b?0:1;else
switch(b[0]){case
0:return[0,a(c,b[1])];case
1:return[1,b[1],b[2],b[3]];case
2:var
d=b[1],e=a_(c,b[2]);return[2,a_(c,d),e];case
3:var
f=b[1],g=a_(c,b[2]);return[3,a_(c,f),g];case
4:return[4,a_(c,b[1])];default:var
h=b[2],i=b[1],j=a_(c,b[3]);return[5,a_(c,i),h,j]}}function
he(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];if(b){var
c=b[1];return[0,c,he(a[3])]}}return 0}var
fj=0;function
hf(I,H,k,j,c){function
h(c,a){return b(l[26],c,a)}function
i(c,e){if(e){var
h=e[2],d=e[1],l=b(j,c[1],d[1]);if(l){if(a(k,l[1]))return[0,[0,c[2],[0,d[2],0]]];var
f=i(c,h);return 0===f[0]?[0,f[1]]:[1,[0,d,f[1]]]}var
g=i(c,h);return 0===g[0]?[0,g[1]]:[1,[0,d,g[1]]]}var
m=b(j,c[1],c[1]);return m?a(k,m[1])?[0,[0,c[2],0]]:[1,[0,c,0]]:[1,[0,c,0]]}function
g(a,d){if(a){var
m=a[1],e=g(a[2],d),j=e[2],k=e[1],h=function(k,f){var
g=f[2],h=f[1],a=m,d=k;for(;;){if(a){var
j=a[2],e=i(a[1],d);if(0!==e[0]){var
a=j,d=e[1];continue}var
c=[0,e[1]]}else
var
c=[1,d];return 0===c[0]?[0,h,b(l[26],g,c[1])]:[0,[0,c[1],h],g]}},c=f(q[21],h,d,n5),n=c[1],o=b(l[26],j,c[2]);return[0,b(l[26],k,n),o]}return[0,fj,0]}function
e(O,N){var
c=O,d=N;for(;;)if(typeof
d==="number")return 0===d?c?[0,fj,0]:[0,c8,0]:c?[0,c8,0]:[0,fj,0];else
switch(d[0]){case
0:return c?[0,c8,0]:[0,c8,0];case
1:var
f=d[2],i=d[1],P=0;if(c)var
L=a(H,i),M=function(a){function
c(a){return[0,a,f]}return b(q[17],c,a)},j=b(q[17],M,L);else
var
J=a(I,i),K=function(a){function
c(a){return[0,a,f]}return b(q[17],c,a)},j=b(q[17],K,J);return[0,j,P];case
2:var
Q=d[2],k=e(c,d[1]),m=k[2],n=k[1],o=e(c,Q),p=o[2],r=o[1];if(c){var
R=b(l[26],m,p);return[0,h(n,r),R]}var
s=g(n,r),S=s[1],T=b(l[26],p,s[2]);return[0,S,b(l[26],m,T)];case
3:var
U=d[2],t=e(c,d[1]),u=t[2],v=t[1],w=e(c,U),x=w[2],y=w[1];if(c){var
z=g(v,y),V=z[1],W=b(l[26],x,z[2]);return[0,V,b(l[26],u,W)]}var
X=b(l[26],u,x);return[0,h(v,y),X];case
4:var
c=1-c,d=d[1];continue;default:var
Y=d[3],A=e(1-c,d[1]),B=A[2],C=A[1],D=e(c,Y),E=D[2],F=D[1];if(c){var
G=g(C,F),Z=G[1],_=b(l[26],E,G[2]);return[0,Z,b(l[26],B,_)]}var
$=b(l[26],B,E);return[0,h(C,F),$]}}return e(1,c)}var
am=a(gr[1],[0,ab]),n7=b(l[26],ci[6],hg),n8=b(l[26],ci[5],n7),n9=b(l[26],[0,n6,0],n8),n_=b(l[26],ci[7],n9);function
H(d,c,b){var
e=f(ci[4],d,c,b),g=a(oc[21],e);return a(j[8],g)}var
od=ci[7];function
aU(a){return H(oe,od,a)}function
u(a){return H(of,n_,a)}function
aV(a){return H(og,n$,a)}function
an(a){return H(oh,oa,a)}function
aW(a){return H(oi,ob,a)}function
aX(a){return H(oj,hg,a)}var
ao=[d,function(a){return aU(ok)}],ap=[d,function(a){return aU(ol)}],a$=[d,function(a){return aU(om)}],c9=[d,function(a){return aU(on)}],ba=[d,function(a){return aU(oo)}],aq=[d,function(a){return aU(op)}],c_=[d,function(a){return u(oq)}],c$=[d,function(a){return u(or)}],bb=[d,function(a){return u(os)}],da=[d,function(a){return aU(ot)}],db=[d,function(a){return aU(ou)}],dc=[d,function(a){return aV(ov)}],dd=[d,function(a){return aV(ow)}],de=[d,function(a){return aV(ox)}],df=[d,function(a){return aV(oy)}],dg=[d,function(a){return aV(oz)}],K=[d,function(a){return aV(oA)}],dh=[d,function(a){return aV(oB)}],di=[d,function(a){return aV(oC)}],dj=[d,function(a){return aV(oD)}],ag=[d,function(a){return u(oE)}],$=[d,function(a){return u(oF)}],bc=[d,function(a){return u(oG)}],bd=[d,function(a){return u(oH)}],dk=[d,function(a){return aX(oI)}],dl=[d,function(a){return aX(oJ)}],dm=[d,function(a){return aX(oK)}],dn=[d,function(a){return aX(oL)}],dp=[d,function(a){return aX(oM)}],dq=[d,function(a){return aX(oN)}],dr=[d,function(a){return aX(oO)}],ds=[d,function(a){return aX(oP)}],dt=[d,function(a){return aX(oQ)}],be=[d,function(a){return u(oR)}],bf=[d,function(a){return u(oS)}],bg=[d,function(a){return u(oT)}],du=[d,function(a){return u(oU)}],dv=[d,function(a){return u(oV)}],dw=[d,function(a){return u(oW)}],dx=[d,function(a){return u(oX)}],oZ=[d,function(a){return aW(oY)}],o1=[d,function(a){return aW(o0)}],o3=[d,function(a){return aW(o2)}],o5=[d,function(a){return aW(o4)}],ar=[d,function(a){return aU(o6)}],cj=[d,function(a){return aW(o7)}],ck=[d,function(a){return aW(o8)}],cl=[d,function(a){return aW(o9)}],cm=[d,function(a){return aW(o_)}],cn=[d,function(a){return aW(o$)}],pb=[d,function(a){return u(pa)}],pd=[d,function(a){return u(pc)}],pf=[d,function(a){return u(pe)}],co=[d,function(a){return u(pg)}],cp=[d,function(a){return u(ph)}],cq=[d,function(a){return u(pi)}],cr=[d,function(a){return u(pj)}],cs=[d,function(a){return u(pk)}],pm=[d,function(a){return an(pl)}],po=[d,function(a){return an(pn)}],pq=[d,function(a){return an(pp)}],ps=[d,function(a){return an(pr)}],aD=[d,function(a){return an(pt)}],aE=[d,function(a){return an(pu)}],aY=[d,function(a){return an(pv)}],aF=[d,function(a){return an(pw)}],bh=[d,function(a){return an(px)}],ct=[d,function(a){return an(py)}],bi=[d,function(a){return an(pz)}],bj=[d,function(a){return an(pA)}],dy=[d,function(a){return u(pB)}],dz=[d,function(a){return u(pC)}],dA=[d,function(a){return u(pD)}],dB=[d,function(a){return u(pE)}],dC=[d,function(a){return u(pF)}],dD=[d,function(a){return u(pG)}],dE=[d,function(a){return u(pH)}],dF=[d,function(a){return u(pI)}],dG=[d,function(a){return u(pJ)}],dH=[d,function(a){return u(pK)}],dI=[d,function(a){return u(pL)}],dJ=[d,function(a){return u(pM)}],dK=[d,function(a){return u(pN)}],dL=[d,function(a){return u(pO)}],dM=[d,function(a){return u(pP)}],dN=[d,function(a){return u(pQ)}],dO=[d,function(a){return u(pR)}],dP=[d,function(a){return u(pS)}],dQ=[d,function(a){return u(pT)}],dR=[d,function(a){return u(pU)}],dS=[d,function(a){return u(pV)}],dT=[d,function(a){return u(pW)}],dU=[d,function(a){return u(pX)}],dV=[d,function(a){return H(p0,pZ,pY)}],dW=[d,function(a){return H(p3,p2,p1)}],dX=[d,function(a){return H(p6,p5,p4)}],dY=[d,function(a){return H(p9,p8,p7)}],dZ=[d,function(a){return H(qa,p$,p_)}],d0=[d,function(a){return H(qd,qc,qb)}],d1=[d,function(a){return H(qg,qf,qe)}],d2=[d,function(a){return H(qj,qi,qh)}],bk=[d,function(a){return H(qm,ql,qk)}],as=[d,function(a){return H(qp,qo,qn)}],d3=[d,function(a){return H(qs,qr,qq)}],bl=[d,function(a){return H(qv,qu,qt)}],L=[bs,qw,bq(0)];function
fk(c,e){var
a=b(j[3],c,e);switch(a[0]){case
9:var
f=a[2],d=b(j[3],c,a[1]);if(12===d[0])return[0,d[1][1][2],f];throw L;case
12:return[0,a[1][1][2],[0]];default:throw L}}function
hh(a,d){var
b=fk(a,d),c=b[1],e=b[2];if(1===c)return 0;if(2===c)return[0,hh(a,B(e,0)[1])];throw L}function
qx(c,b){var
d=a(k[8][5],b);return f(r[1],c,qy,d)}function
fl(b){if(b){var
c=h(db),f=[0,fl(b[1])],k=i===c?db[1]:d===c?a(g[2],db):db;return a(j[21],[0,k,f])}var
e=h(da);return i===e?da[1]:d===e?a(g[2],da):da}function
cu(a,e){var
b=fk(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw L;switch(d){case
0:return[0,cu(a,B(c,0)[1])];case
1:return[1,cu(a,B(c,0)[1])];default:return 0}}function
aZ(b){if(typeof
b==="number"){var
c=h(de);return i===c?de[1]:d===c?a(g[2],de):de}else{if(0===b[0]){var
e=h(dg),k=[0,aZ(b[1])],l=i===e?dg[1]:d===e?a(g[2],dg):dg;return a(j[21],[0,l,k])}var
f=h(df),m=[0,aZ(b[1])],n=i===f?df[1]:d===f?a(g[2],df):df;return a(j[21],[0,n,m])}}function
fm(c,b){var
d=a(k[8][3],b);return f(r[1],c,qz,d)}function
cv(a,e){var
b=fk(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw L;switch(d){case
0:return 0;case
1:return[0,cu(a,B(c,0)[1])];default:return[1,cu(a,B(c,0)[1])]}}function
aA(b){if(typeof
b==="number"){var
c=h(dh);return i===c?dh[1]:d===c?a(g[2],dh):dh}else{if(0===b[0]){var
e=h(di),k=[0,aZ(b[1])],l=i===e?di[1]:d===e?a(g[2],di):di;return a(j[21],[0,l,k])}var
f=h(dj),m=[0,aZ(b[1])],n=i===f?dj[1]:d===f?a(g[2],dj):dj;return a(j[21],[0,n,m])}}function
bm(c,b){var
d=a(k[8][1],b),e=a(o[33],d);return f(r[1],c,qA,e)}function
bD(b){var
e=aZ(b[2]),c=h(bc),f=[0,aA(b[1]),e],k=i===c?bc[1]:d===c?a(g[2],bc):bc;return a(j[21],[0,k,f])}function
hi(c,m){var
e=b(j[3],c,m);if(9===e[0]){var
k=e[2],l=h(bc),n=e[1],o=i===l?bc[1]:d===l?a(g[2],bc):bc;if(f(j[95],c,n,o)){var
p=cu(c,B(k,1)[2]);return[0,cv(c,B(k,0)[1]),p]}throw L}throw L}function
a0(b){if(typeof
b==="number"){if(0===b){var
c=h(dk);return i===c?dk[1]:d===c?a(g[2],dk):dk}var
e=h(dl);return i===e?dl[1]:d===e?a(g[2],dl):dl}else
switch(b[0]){case
0:var
f=h(dm),q=[0,bD(b[1])],r=i===f?dm[1]:d===f?a(g[2],dm):dm;return a(j[21],[0,r,q]);case
1:var
k=h(dn),s=[0,aA(b[1])],t=i===k?dn[1]:d===k?a(g[2],dn):dn;return a(j[21],[0,t,s]);case
2:var
u=b[1],v=a0(b[2]),l=h(dp),w=[0,a0(u),v],x=i===l?dp[1]:d===l?a(g[2],dp):dp;return a(j[21],[0,x,w]);case
3:var
y=b[1],z=a0(b[2]),m=h(dq),A=[0,a0(y),z],B=i===m?dq[1]:d===m?a(g[2],dq):dq;return a(j[21],[0,B,A]);case
4:var
C=b[1],D=a0(b[2]),n=h(dr),E=[0,a0(C),D],F=i===n?dr[1]:d===n?a(g[2],dr):dr;return a(j[21],[0,F,E]);case
5:var
o=h(ds),G=[0,a0(b[1])],H=i===o?ds[1]:d===o?a(g[2],ds):ds;return a(j[21],[0,H,G]);default:var
p=h(dt),I=[0,a0(b[1])],J=i===p?dt[1]:d===p?a(g[2],dt):dt;return a(j[21],[0,J,I])}}function
fn(c,e,b){if(b){var
l=b[1],m=fn(c,e,b[2]),f=h(c_),n=[0,c,a(e,l),m],o=i===f?c_[1]:d===f?a(g[2],c_):c_;return a(j[21],[0,o,n])}var
k=h(c$),p=[0,c],q=i===k?c$[1]:d===k?a(g[2],c$):c$;return a(j[21],[0,q,p])}function
hj(e,w,b){function
c(b){switch(b[0]){case
0:var
l=h(dz),x=[0,e,a(w,b[1])],y=i===l?dz[1]:d===l?a(g[2],dz):dz;return a(j[21],[0,y,x]);case
1:var
m=h(dy),z=[0,e,aZ(b[1])],A=i===m?dy[1]:d===m?a(g[2],dy):dy;return a(j[21],[0,A,z]);case
2:var
B=b[1],C=c(b[2]),n=h(dA),D=[0,e,c(B),C],E=i===n?dA[1]:d===n?a(g[2],dA):dA;return a(j[21],[0,E,D]);case
3:var
F=b[1],G=c(b[2]),o=h(dD),H=[0,e,c(F),G],I=i===o?dD[1]:d===o?a(g[2],dD):dD;return a(j[21],[0,I,H]);case
4:var
J=b[1],K=c(b[2]),p=h(dC),L=[0,e,c(J),K],M=i===p?dC[1]:d===p?a(g[2],dC):dC;return a(j[21],[0,M,L]);case
5:var
q=h(dB),N=[0,e,c(b[1])],O=i===q?dB[1]:d===q?a(g[2],dB):dB;return a(j[21],[0,O,N]);default:var
r=b[2],P=b[1];if(r)var
f=h(dd),u=[0,aZ(r[1])],v=i===f?dd[1]:d===f?a(g[2],dd):dd,s=a(j[21],[0,v,u]);else
var
k=h(dc),s=i===k?dc[1]:d===k?a(g[2],dc):dc;var
t=h(dE),Q=[0,e,c(P),s],R=i===t?dE[1]:d===t?a(g[2],dE):dE;return a(j[21],[0,R,Q])}}return c(b)}function
hk(e,m,b){function
c(b){switch(b[0]){case
0:var
f=h(dG),n=[0,e,a(m,b[1])],o=i===f?dG[1]:d===f?a(g[2],dG):dG;return a(j[21],[0,o,n]);case
1:var
p=b[1],q=c(b[2]),k=h(dH),r=[0,e,aZ(p),q],s=i===k?dH[1]:d===k?a(g[2],dH):dH;return a(j[21],[0,s,r]);default:var
t=b[2],u=b[1],v=c(b[3]),w=aZ(t),l=h(dF),x=[0,e,c(u),w,v],y=i===l?dF[1]:d===l?a(g[2],dF):dF;return a(j[21],[0,y,x])}}return c(b)}function
aG(d,c,a){function
b(c,a){switch(a[0]){case
0:return aa(r[1],c,qE,d,a[1]);case
1:return M(r[1],c,qF,fm,a[1],b,a[2]);default:return em(r[1],c,qG,b,a[1],fm,a[2],b,a[3])}}return b(c,a)}function
bR(b,f,l){var
k=h(b),c=i===k?b[1]:d===k?a(g[2],b):b;function
e(b){if(typeof
b==="number"){var
k=h(dU),r=[0,c],s=i===k?dU[1]:d===k?a(g[2],dU):dU;return a(j[21],[0,s,r])}else
switch(b[0]){case
0:var
l=h(dO),t=[0,c,fl(b[1])],u=i===l?dO[1]:d===l?a(g[2],dO):dO;return a(j[21],[0,u,t]);case
1:var
m=h(dP),v=[0,c,hk(c,f,b[1])],w=i===m?dP[1]:d===m?a(g[2],dP):dP;return a(j[21],[0,w,v]);case
2:var
x=b[1],y=e(b[2]),n=h(dR),z=[0,c,hk(c,f,x),y],A=i===n?dR[1]:d===n?a(g[2],dR):dR;return a(j[21],[0,A,z]);case
3:var
B=b[1],C=e(b[2]),o=h(dQ),D=[0,c,e(B),C],E=i===o?dQ[1]:d===o?a(g[2],dQ):dQ;return a(j[21],[0,E,D]);case
4:var
F=b[1],G=e(b[2]),p=h(dS),H=[0,c,e(F),G],I=i===p?dS[1]:d===p?a(g[2],dS):dS;return a(j[21],[0,I,H]);default:var
q=h(dT),J=[0,c,a(f,b[1])],K=i===q?dT[1]:d===q?a(g[2],dT):dT;return a(j[21],[0,K,J])}}return e(l)}function
a1(e,c,a){function
d(c,a){if(typeof
a==="number")return b(r[1],c,qH);else
switch(a[0]){case
0:return aa(r[1],c,qI,qx,a[1]);case
1:var
f=a[1],g=function(a,b){return aG(e,a,b)};return aa(r[1],c,qJ,g,f);case
2:var
h=a[2],i=a[1],j=function(a,b){return aG(e,a,b)};return M(r[1],c,qK,j,i,d,h);case
3:return M(r[1],c,qL,d,a[1],d,a[2]);case
4:return M(r[1],c,qM,d,a[1],d,a[2]);default:return aa(r[1],c,qN,e,a[1])}}return d(c,a)}function
hl(e,p,c){var
r=c[2],s=c[1],t=hj(e,p,c[3]);switch(r){case
0:var
f=h(dI),b=i===f?dI[1]:d===f?a(g[2],dI):dI;break;case
1:var
k=h(dJ),b=i===k?dJ[1]:d===k?a(g[2],dJ):dJ;break;case
2:var
l=h(dK),b=i===l?dK[1]:d===l?a(g[2],dK):dK;break;case
3:var
m=h(dM),b=i===m?dM[1]:d===m?a(g[2],dM):dM;break;case
4:var
n=h(dL),b=i===n?dL[1]:d===n?a(g[2],dL):dL;break;default:var
o=h(dN),b=i===o?dN[1]:d===o?a(g[2],dN):dN}var
q=h(d3),u=[0,e,hj(e,p,s),b,t],v=i===q?d3[1]:d===q?a(g[2],d3):d3;return a(j[21],[0,v,u])}function
d4(k,e,c){try{var
l=function(l){var
b=l[1],c=h(b),m=i===c?b[1]:d===c?a(g[2],b):b;return f(j[95],k,e,m)},m=b(q[33],l,c)[2];return m}catch(a){a=w(a);if(a===J)throw L;throw a}}var
hm=[0,[0,oZ,5],[0,[0,o1,3],[0,[0,o5,4],[0,[0,o3,2],0]]]],hn=[0,[0,pm,5],[0,[0,po,3],[0,[0,ps,4],[0,[0,pq,2],0]]]],ho=[0,[0,pd,4],[0,[0,pb,2],[0,[0,pf,0],0]]];function
hp(a,c,b){return V(qO[80],0,a[1],a[2],c,b)}function
qP(n,m){var
c=m[2],e=m[1],k=n[2],o=b(j[3],k,e);switch(o[0]){case
10:var
r=B(c,1)[2],s=B(c,0)[1];return[0,d4(k,e,hm),s,r];case
11:if(0===o[1][1][2]){var
p=h(ar),t=i===p?ar[1]:d===p?a(g[2],ar):ar;if(f(j[95],k,e,t)){var
q=h(K),u=i===q?K[1]:d===q?a(g[2],K):K;if(hp(n,B(c,0)[1],u)){var
v=B(c,2)[3];return[0,0,B(c,1)[2],v]}}throw L}break}return a(l[3],qQ)}function
qR(n,m){var
c=m[2],e=m[1],k=n[2],o=b(j[3],k,e);switch(o[0]){case
10:var
r=B(c,1)[2],s=B(c,0)[1];return[0,d4(k,e,hn),s,r];case
11:if(0===o[1][1][2]){var
p=h(ar),t=i===p?ar[1]:d===p?a(g[2],ar):ar;if(f(j[95],k,e,t)){var
q=h($),u=i===q?$[1]:d===q?a(g[2],$):$;if(hp(n,B(c,0)[1],u)){var
v=B(c,2)[3];return[0,0,B(c,1)[2],v]}}throw L}break}return a(l[3],qS)}function
qT(c,a){var
b=a[2],d=a[1],e=B(b,1)[2],f=B(b,0)[1];return[0,d4(c[2],d,ho),f,e]}function
d5(e,h,c){function
d(a,c,b){if(a){var
e=a[1],i=a[2];if(f(j[96],h,e,b))return[0,a,c];var
g=d(i,c+1|0,b);return[0,[0,e,g[1]],g[2]]}return[0,[0,b,0],c]}var
b=d(e,1,c),g=b[1];return[0,g,a(k[7][1],b[2])]}function
hq(e,d,c){var
a=e,b=1;for(;;){if(a){var
g=a[2];if(f(j[95],d,a[1],c))return b;var
a=g,b=b+1|0;continue}throw[0,qW,qV]}}var
hr=0;function
fo(e,o,C,A,k,c){function
r(c,b){var
a=d5(c,e,b);return[0,[1,a[2]],a[1]]}function
l(c,k){function
D(f,e,a){var
g=a[2],c=l(f,a[1]),h=c[1],d=l(c[2],g),i=d[2];return[0,b(e,h,d[1]),i]}try{var
K=[0,[0,a(o,k)],c];return K}catch(o){o=w(o);if(o===L){var
p=b(j[3],e,k);if(9===p[0]){var
m=p[2],s=p[1];if(10===b(j[3],e,s)[0]){try{var
y=function(k){var
b=k[1],c=h(b),l=i===c?b[1]:d===c?a(g[2],b):b;return f(j[95],e,s,l)},z=b(q[33],y,A)[2],n=z}catch(a){a=w(a);if(a!==J)throw a;var
n=qU}if(typeof
n==="number"){if(0===n){var
t=l(c,B(m,0)[1]);return[0,[5,t[1]],t[2]]}try{var
v=l(c,B(m,0)[1]),E=v[2],F=v[1],G=[0,b(C,F,B(m,1)[2]),E];return G}catch(b){b=w(b);if(a(a7[18],b)){var
u=d5(c,e,k);return[0,[1,u[2]],u[1]]}throw b}}else{if(0===n[0]){var
H=n[1],I=B(m,1)[2];return D(c,H,[0,B(m,0)[1],I])}var
x=d5(c,e,k);return[0,[1,x[2]],x[1]]}}return r(c,k)}return r(c,k)}throw o}}return l(k,c)}var
qY=[0,[0,cl,0],[0,[0,cn,1],0]],qZ=[0,[0,cm,[0,function(b,a){return[4,b,a]}]],qY],q0=[0,[0,ck,[0,function(b,a){return[3,b,a]}]],qZ],q1=[0,[0,cj,[0,function(b,a){return[2,b,a]}]],q0],q2=[0,[0,cq,0],[0,[0,cs,1],0]],q3=[0,[0,cr,[0,function(b,a){return[4,b,a]}]],q2],q4=[0,[0,cp,[0,function(b,a){return[3,b,a]}]],q3],q5=[0,[0,co,[0,function(b,a){return[2,b,a]}]],q4],q6=[0,[0,aY,0],[0,[0,ct,1],0]],q7=[0,[0,aF,[0,function(b,a){return[4,b,a]}]],q6],q8=[0,[0,aE,[0,function(b,a){return[3,b,a]}]],q7],q9=[0,[0,aD,[0,function(b,a){return[2,b,a]}]],q8],q_=0,q$=[0,[0,aF,function(b,a){return[4,b,a]}],q_],ra=[0,[0,aE,function(b,a){return[3,b,a]}],q$],rb=[0,[0,aD,function(b,a){return[2,b,a]}],ra];function
d6(c,l){var
m=b(j[3],c,l);switch(m[0]){case
9:var
e=m[2],k=m[1];try{var
z=d4(c,k,rb),A=d6(c,B(e,0)[1]),C=b(z,A,d6(c,B(e,1)[2]));return C}catch(l){l=w(l);if(l===L){var
n=h(bh),u=i===n?bh[1]:d===n?a(g[2],bh):bh;if(f(j[95],c,k,u)){var
o=d6(c,B(e,0)[1]),v=a(p[fB],o);if(b(p[77],v,rc))throw L;return[5,o]}var
q=h(bj),x=i===q?bj[1]:d===q?a(g[2],bj):bj;if(f(j[95],c,k,x))return[0,hi(c,B(e,0)[1])];var
r=h(bi),y=i===r?bi[1]:d===r?a(g[2],bi):bi;if(f(j[95],c,k,y))return[1,cv(c,B(e,0)[1])];throw L}throw l}case
10:var
s=h(be),D=i===s?be[1]:d===s?a(g[2],be):be;if(f(j[95],c,l,D))return 0;var
t=h(bf),E=i===t?bf[1]:d===t?a(g[2],bf):bf;if(f(j[95],c,l,E))return 1;throw L;default:throw L}}function
rd(b){function
c(e,d){var
c=cv(b,d);if(typeof
c!=="number"&&1===c[0])return re;return[6,e,a(p[12][15],c)]}function
d(a){return cv(b,a)}return function(a,e){return fo(b,d,c,q1,a,e)}}function
rf(d){function
c(e,f){var
c=cv(d,f);if(typeof
c!=="number"&&1===c[0]){if(0===e[0])return[0,b(p[85],e[1],c)];a(l[31],rg);a(l[52],l[28]);throw L}return[6,e,a(p[12][15],c)]}function
e(a){return hi(d,a)}return function(a,b){return fo(d,e,c,q5,a,b)}}function
rh(c){function
d(d,b){var
e=hh(c,b);return[6,d,a(p[7][1],e)]}function
e(a){b(qX[6],0,0);return d6(c,a)}return function(a,b){return fo(c,e,d,q9,a,b)}}function
fp(o,h,n,m,g){var
c=g[2],d=b(j[3],c,m);if(9===d[0]){var
e=b(o,g,[0,d[1],d[2]]),p=e[3],q=e[1],i=f(h,c,n,e[2]),r=i[1],k=f(h,c,i[2],p);return[0,[0,r,q,k[1]],k[2]]}return a(l[3],ri)}function
d7(a,b,c){return fp(qP,rd,a,b,c)}function
d8(a,b,c){return fp(qT,rf,a,b,c)}function
rj(a,b,c){return fp(qR,rh,a,b,c)}function
rk(b,a){return[2,b,a]}function
rl(b,a){return[3,b,a]}function
rm(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
rn(b,a){return[5,b,0,a]}function
d9(e,d,c,a){if(typeof
c!=="number"&&0===c[0])if(typeof
a!=="number"&&0===a[0])return[0,d];return b(e,c,a)}function
hs(o,p,n,l,e){var
m=o[2];function
K(e,d,c){try{var
b=f(p,e,c,o),g=b[2],h=b[1],i=[0,[1,h,d,c],g,a(k[4][2],d)];return i}catch(b){b=w(b);if(a(a7[18],b))return[0,[0,c],e,d];throw b}}function
c(l,k,e){var
n=b(j[3],m,e);switch(n[0]){case
6:var
E=n[3],P=n[2];if(f(j[108][13],m,1,E)){var
r=c(l,k,P),Q=r[1],s=c(r[2],r[3],E),R=s[3],S=s[2];return[0,d9(rn,e,Q,s[1]),S,R]}break;case
9:var
p=n[2],q=n[1],F=p.length-1;if(!(3<=F))switch(F){case
0:break;case
1:var
G=h(a$),T=p[1],U=i===G?a$[1]:d===G?a(g[2],a$):a$;if(f(j[95],m,q,U)){var
t=c(l,k,T);return[0,[4,t[1]],t[2],t[3]]}break;default:var
u=p[1],v=p[2],H=h(ao),V=i===H?ao[1]:d===H?a(g[2],ao):ao;if(f(j[95],m,q,V)){var
w=c(l,k,u),W=w[1],x=c(w[2],w[3],v),X=x[3],Y=x[2];return[0,d9(rk,e,W,x[1]),Y,X]}var
I=h(ap),Z=i===I?ap[1]:d===I?a(g[2],ap):ap;if(f(j[95],m,q,Z)){var
y=c(l,k,u),_=y[1],z=c(y[2],y[3],v),$=z[3],ab=z[2];return[0,d9(rl,e,_,z[1]),ab,$]}var
J=h(c9),ac=i===J?c9[1]:d===J?a(g[2],c9):c9;if(f(j[95],m,q,ac)){var
A=c(l,k,u),ad=A[1],B=c(A[2],A[3],v),ae=B[3],af=B[2];return[0,d9(rm,e,ad,B[1]),af,ae]}}return K(l,k,e)}var
C=h(ba),N=i===C?ba[1]:d===C?a(g[2],ba):ba;if(f(j[95],m,e,N))return[0,0,l,k];var
D=h(aq),O=i===D?aq[1]:d===D?a(g[2],aq):aq;if(f(j[95],m,e,O))return[0,1,l,k];var
M=aa(ro[3],0,o[1],o[2],e);if(a(rp[8],M))return[0,[0,e],l,k];throw L}return c(n,l,e)}function
ht(c,r,b){function
e(b){if(typeof
b==="number"){if(0===b){var
f=h(dV),s=[0,c],t=i===f?dV[1]:d===f?a(g[2],dV):dV;return a(j[21],[0,t,s])}var
k=h(dW),u=[0,c],v=i===k?dW[1]:d===k?a(g[2],dW):dW;return a(j[21],[0,v,u])}else
switch(b[0]){case
0:var
l=h(d1),w=[0,c,b[1]],x=i===l?d1[1]:d===l?a(g[2],d1):d1;return a(j[21],[0,x,w]);case
1:var
m=h(d0),y=[0,c,a(r,b[1])],z=i===m?d0[1]:d===m?a(g[2],d0):d0;return a(j[21],[0,z,y]);case
2:var
A=b[1],B=e(b[2]),n=h(dX),C=[0,c,e(A),B],D=i===n?dX[1]:d===n?a(g[2],dX):dX;return a(j[21],[0,D,C]);case
3:var
E=b[1],F=e(b[2]),o=h(dY),G=[0,c,e(E),F],H=i===o?dY[1]:d===o?a(g[2],dY):dY;return a(j[21],[0,H,G]);case
4:var
p=h(dZ),I=[0,c,e(b[1])],J=i===p?dZ[1]:d===p?a(g[2],dZ):dZ;return a(j[21],[0,J,I]);default:var
K=b[1],L=e(b[3]),q=h(d2),M=[0,c,e(K),L],N=i===q?d2[1]:d===q?a(g[2],d2):d2;return a(j[21],[0,N,M])}}return e(b)}function
fq(g,a){function
d(i,h){var
b=i,a=h;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
0:return d5(b,g,a[1])[1];case
4:var
a=a[1];continue;case
5:var
f=a[3],e=a[1],c=1;break;case
1:var
c=0;break;default:var
f=a[2],e=a[1],c=1}if(c){var
b=d(b,e),a=f;continue}return b}}return d(0,a)}var
d_=[d,function(x){function
n(c){var
b=c[1],e=h(b),f=c[2],j=i===e?b[1]:d===e?a(g[2],b):b;return[0,f,j]}var
o=b(q[17],n,hm),c=h(cn);function
p(b){var
c=a(k[8][4],b);return aA(a(k[7][7],c))}var
r=i===c?cn[1]:d===c?a(g[2],cn):cn,e=h(cm),s=i===e?cm[1]:d===e?a(g[2],cm):cm,f=h(cl),t=i===f?cl[1]:d===f?a(g[2],cl):cl,j=h(ck),u=i===j?ck[1]:d===j?a(g[2],ck):ck,l=h(cj),v=i===l?cj[1]:d===l?a(g[2],cj):cj,m=h(K),w=i===m?K[1]:d===m?a(g[2],K):K;return[0,w,aA,v,u,t,s,r,p,o]}],d$=[d,function(x){function
n(c){var
b=c[1],e=h(b),f=c[2],j=i===e?b[1]:d===e?a(g[2],b):b;return[0,f,j]}var
o=b(q[17],n,ho),c=h(cs);function
p(b){var
c=a(k[8][4],b);return aA(a(k[7][7],c))}var
r=i===c?cs[1]:d===c?a(g[2],cs):cs,e=h(cr),s=i===e?cr[1]:d===e?a(g[2],cr):cr,f=h(cq),t=i===f?cq[1]:d===f?a(g[2],cq):cq,j=h(cp),u=i===j?cp[1]:d===j?a(g[2],cp):cp,l=h(co),v=i===l?co[1]:d===l?a(g[2],co):co,m=h(ag),w=i===m?ag[1]:d===m?a(g[2],ag):ag;return[0,w,bD,v,u,t,s,r,p,o]}];function
a2(b){if(typeof
b==="number"){if(0===b){var
c=h(be);return i===c?be[1]:d===c?a(g[2],be):be}var
e=h(bf);return i===e?bf[1]:d===e?a(g[2],bf):bf}else
switch(b[0]){case
0:var
f=h(bj),q=[0,bD(b[1])],r=i===f?bj[1]:d===f?a(g[2],bj):bj;return a(j[21],[0,r,q]);case
1:var
k=h(bi),s=[0,aA(b[1])],t=i===k?bi[1]:d===k?a(g[2],bi):bi;return a(j[21],[0,t,s]);case
2:var
u=b[1],v=a2(b[2]),l=h(aD),w=[0,a2(u),v],x=i===l?aD[1]:d===l?a(g[2],aD):aD;return a(j[21],[0,x,w]);case
3:var
y=b[1],z=a2(b[2]),m=h(aE),A=[0,a2(y),z],B=i===m?aE[1]:d===m?a(g[2],aE):aE;return a(j[21],[0,B,A]);case
4:var
C=b[1],D=a2(b[2]),n=h(aF),E=[0,a2(C),D],F=i===n?aF[1]:d===n?a(g[2],aF):aF;return a(j[21],[0,F,E]);case
5:var
o=h(bh),G=[0,a2(b[1])],H=i===o?bh[1]:d===o?a(g[2],bh):bh;return a(j[21],[0,H,G]);default:var
p=h(aY),I=[0,a2(b[1])],J=i===p?aY[1]:d===p?a(g[2],aY):aY;return a(j[21],[0,J,I])}}var
ea=[d,function(x){function
n(c){var
b=c[1],e=h(b),f=c[2],j=i===e?b[1]:d===e?a(g[2],b):b;return[0,f,j]}var
o=b(q[17],n,hn),c=h(ct);function
p(b){var
c=a(k[8][4],b);return fl(a(k[7][4],c))}var
r=i===c?ct[1]:d===c?a(g[2],ct):ct,e=h(aF),s=i===e?aF[1]:d===e?a(g[2],aF):aF,f=h(aY),t=i===f?aY[1]:d===f?a(g[2],aY):aY,j=h(aE),u=i===j?aE[1]:d===j?a(g[2],aE):aE,l=h(aD),v=i===l?aD[1]:d===l?a(g[2],aD):aD,m=h($),w=i===m?$[1]:d===m?a(g[2],$):$;return[0,w,a2,v,u,t,s,r,p,o]}];function
hu(h,g,f){var
b=[0,h,g,f];for(;;){var
d=b[1];if(0===d)return b[3];var
c=b[2];if(c){var
e=c[1],i=c[2],b=[0,d-1|0,i,a(j[18],[0,e[1],e[2],b[3]])];continue}throw[0,ay,rq]}}function
hv(p,e,c){function
m(d){var
c=d;for(;;)switch(c[0]){case
0:return am[1];case
1:var
e=a(k[8][3],c[1]);return a(am[5],e);case
5:case
6:var
c=c[1];continue;default:var
f=c[1],g=m(c[2]),h=m(f);return b(am[7],h,g)}}function
o(j){var
a=j;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
1:var
d=a[1],g=d[1],h=m(d[3]),i=m(g);return b(am[7],i,h);case
4:var
a=a[1];continue;case
5:var
f=a[3],e=a[1],c=1;break;case
0:var
c=0;break;default:var
f=a[2],e=a[1],c=1}if(c){var
k=o(f),l=o(e);return b(am[7],l,k)}return am[1]}}var
x=o(c),y=a(am[21],x);function
z(b,a){return[0,a,b+1|0]}var
s=b(q[18],z,y),t=fq(p,c);function
A(c){var
d=e[1],f=b(r[4],rr,c[2]);return[0,a(a9[1][6],f),d]}var
n=b(q[17],A,s);function
B(c,f){var
d=j[14],e=b(r[4],rs,c+1|0);return[0,a(a9[1][6],e),d]}var
u=b(q[18],B,t);function
C(b,a){return[0,a[1],b[1]]}var
D=f(q[23],C,s,n);function
v(f,c){function
d(c){switch(c[0]){case
0:return a(e[2],c[1]);case
1:var
g=a(k[8][3],c[1]),h=f+b(q[38],g,s)|0;return a(j[9],h);case
2:var
i=c[1],l=d(c[2]),m=[0,d(i),l];return a(j[21],[0,e[3],m]);case
3:var
n=c[1],o=d(c[2]),p=[0,d(n),o];return a(j[21],[0,e[4],p]);case
4:var
r=c[1],t=d(c[2]),u=[0,d(r),t];return a(j[21],[0,e[6],u]);case
5:var
v=[0,d(c[1])];return a(j[21],[0,e[5],v]);default:var
w=c[1],x=a(e[8],c[2]),y=[0,d(w),x];return a(j[21],[0,e[7],y])}}return d(c)}function
E(l,f,c){try{var
o=[0,b(q[38],l,e[9]),[0,f,c]],p=a(j[21],o);return p}catch(b){b=w(b);if(b===J){var
k=h(ar),m=[0,e[1],f,c],n=i===k?ar[1]:d===k?a(g[2],ar):ar;return a(j[21],[0,n,m])}throw b}}function
l(f,e,c){if(typeof
c==="number"){if(0===c){var
m=h(ba);return i===m?ba[1]:d===m?a(g[2],ba):ba}var
n=h(aq);return i===n?aq[1]:d===n?a(g[2],aq):aq}else
switch(c[0]){case
0:var
x=f+hq(t,p,c[1])|0;return a(j[9],x);case
1:var
k=c[1],s=k[2],u=k[1],w=v(e,k[3]);return E(s,v(e,u),w);case
2:var
y=c[1],z=l(f,e,c[2]),o=h(ao),A=[0,l(f,e,y),z],B=i===o?ao[1]:d===o?a(g[2],ao):ao;return a(j[21],[0,B,A]);case
3:var
C=c[1],D=l(f,e,c[2]),q=h(ap),F=[0,l(f,e,C),D],G=i===q?ap[1]:d===q?a(g[2],ap):ap;return a(j[21],[0,G,F]);case
4:var
r=h(aq),H=c[1],I=i===r?aq[1]:d===r?a(g[2],aq):aq,J=l(f,e,H);return b(j[33],J,I);default:var
K=c[1],L=l(f+1|0,e+1|0,c[3]),M=l(f,e,K);return b(j[33],M,L)}}var
F=a(q[1],n),G=a(q[1],u),H=a_(function(c){var
d=hq(t,p,c),e=b(r[4],rt,d),f=a(a9[1][6],e);return a(j[10],f)},c),I=a(q[9],D),K=a(q[9],u),L=l(a(q[1],n),0,c);function
M(a){return[0,[0,a[1]],a[2]]}var
N=hu(F,b(q[17],M,n),L);function
O(a){return[0,[0,a[1]],a[2]]}return[0,hu(G,b(q[17],O,u),N),K,I,H]}function
hw(f,e){var
c=e,b=f;for(;;){if(b){var
d=b[1],g=b[2],h=d[3],i=d[2],k=a(a9[1][6],d[1]),c=aa(j[40],k,i,h,c),b=g;continue}return c}}var
eb=[d,function(a){return H(rw,rv,ru)}],ec=[d,function(a){return H(rz,ry,rx)}],ed=[d,function(a){return H(rC,rB,rA)}],ee=[d,function(a){return H(rF,rE,rD)}];function
ef(c,b){if(typeof
b==="number"){var
e=h(ed),l=[0,c],m=i===e?ed[1]:d===e?a(g[2],ed):ed;return a(j[21],[0,m,l])}else{if(0===b[0]){var
f=h(ec),n=[0,c,b[1]],o=i===f?ec[1]:d===f?a(g[2],ec):ec;return a(j[21],[0,o,n])}var
p=b[2],q=b[1],r=ef(c,b[3]),k=h(eb),s=[0,c,ef(c,q),p,r],t=i===k?eb[1]:d===k?a(g[2],eb):eb;return a(j[21],[0,t,s])}}function
hx(b){if(b){var
c=b[1][1],d=0,e=function(d,b){var
e=b[1],f=a(k[7][1],b[2]);return aa(p[88],c,f,e,d)};return f(q[20],e,d,b)}return 0}function
eg(b){if(typeof
b==="number"){var
c=h(du);return i===c?du[1]:d===c?a(g[2],du):du}else
switch(b[0]){case
0:var
m=b[1],n=eg(b[2]),e=h(dv),o=[0,bR(K,aA,m),n],p=i===e?dv[1]:d===e?a(g[2],dv):dv;return a(j[21],[0,p,o]);case
1:var
q=b[1],r=eg(b[2]),f=h(dw),s=[0,bR(K,aA,q),r],t=i===f?dw[1]:d===f?a(g[2],dw):dw;return a(j[21],[0,t,s]);default:var
k=h(bg),u=b[3],v=b[2],w=b[1],x=i===k?bg[1]:d===k?a(g[2],bg):bg,y=fn(x,eg,u),z=bR(K,aA,v),l=h(dx),A=[0,bR(K,aA,w),z,y],B=i===l?dx[1]:d===l?a(g[2],dx):dx;return a(j[21],[0,B,A])}}function
hy(a){return eg(a)}function
aH(b,a){return M(r[1],b,rG,bm,a[1],fm,a[2])}function
bE(c,a){if(typeof
a==="number")return b(r[1],c,rH);else
switch(a[0]){case
0:var
d=a[2],e=a[1],f=function(a,b){return a1(bm,a,b)};return M(r[1],c,rI,f,e,bE,d);case
1:var
g=a[2],h=a[1],i=function(a,b){return a1(bm,a,b)};return M(r[1],c,rJ,i,h,bE,g);default:var
j=a[3],k=a[2],l=a[1],m=function(a,c){function
b(c,a){if(a){var
d=a[2],e=a[1];return d?M(r[1],c,qB,bE,e,b,d):aa(r[1],c,qC,bE,e)}return 0}return M(r[1],a,qD,rL,b,c,rK)},n=function(a,b){return a1(bm,a,b)},o=function(a,b){return a1(bm,a,b)};return em(r[1],c,rM,o,l,n,k,m,j)}}function
hz(h,g,f,e,b){if(b){var
i=b[1],m=i[2],n=i[1],c=hz(h,g,f,e,b[2]),j=c[3],k=c[2],l=c[1];try{var
d=hs(h,g,k,j,m),o=[0,[0,[0,n,d[1]],l],d[2],d[3]];return o}catch(b){b=w(b);if(a(a7[18],b))return[0,l,k,j];throw b}}return[0,0,f,e]}function
hA(d,c,h,g,f){var
b=hs(d,c,h,a(k[4][3],0),f),i=b[1],e=hz(d,c,b[2],b[3],g);return[0,e[1],i,e[2]]}var
eh=[d,function(l){var
b=h(bg),f=i===b?bg[1]:d===b?a(g[2],bg):bg,c=h(K),j=i===c?K[1]:d===c?a(g[2],K):K,e=h(K),k=i===e?K[1]:d===e?a(g[2],K):K;return[0,k,j,aA,f,hy]}],ei=[d,function(m){var
b=h(as);function
f(a){return bR(ag,bD,a)}var
j=i===b?as[1]:d===b?a(g[2],as):as,c=h(ag),k=i===c?ag[1]:d===c?a(g[2],ag):ag,e=h(ag),l=i===e?ag[1]:d===e?a(g[2],ag):ag;return[0,l,k,bD,j,f]}];function
rN(b,m,l,k,f){var
c=h(bl),n=[0,b[2]],o=i===c?bl[1]:d===c?a(g[2],bl):bl,e=a(j[21],[0,o,n]),p=b[3],q=b[2],r=ht(e,function(a){return hl(q,p,a)},f),s=hx(k),t=ef(b[1],s);function
u(k){var
o=a(aI[42][6],k),c=h(ee),n=0,p=[0,[0,rO,m,l],0],q=[0,b[1]],s=i===c?ee[1]:d===c?a(g[2],ee):ee,f=h(bk),u=[0,[0,rP,t,a(j[21],[0,s,q])],p],v=[0,e],w=i===f?bk[1]:d===f?a(g[2],bk):bk,x=hw([0,[0,rQ,r,a(j[21],[0,w,v])],u],o),y=[0,a(ah[53],x),n];return a(F[66][22],y)}return a(cw[67][8],u)}function
bo(c,b){if(typeof
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
d=b[1],e=c[1],f=bo(c[2],b[2]);return[2,bo(e,d),f]}break;case
3:if(typeof
b!=="number"&&3===b[0]){var
g=b[1],h=c[1],i=bo(c[2],b[2]);return[3,bo(h,g),i]}break;case
4:if(typeof
b!=="number"&&4===b[0])return[4,bo(c[1],b[1])];break;default:if(typeof
b!=="number"&&5===b[0]){var
j=b[2],k=b[1],m=c[1],n=bo(c[3],b[3]);return[5,bo(m,k),j,n]}}return a(l[3],rV)}var
fr=[bs,rW,bq(0)];function
hB(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return f(q[21],d,b,c)}function
hC(u,t,s,r,p,F,Q,E,D,P){var
v=hB(E,D)[1],x=hf(u,t,s,r,v),m=x[1],G=x[2];function
n(c){if(c){var
j=c[2],l=c[1],f=function(b){var
c=b[1];return[0,function(d){var
e=[0,a(b[2],0),d],c=a(b[3],e);return c?[0,[0,c[1],b]]:0},c]},g=b(q[17],f,F),h=function(a){return a[1]},i=b(q[17],h,l),d=b(k[12],g,i);if(d){var
m=d[1],e=n(j);return e?[0,[0,m,e[1]]]:0}return 0}return rR}var
o=n(m);if(o){var
y=o[1],H=b(q[47],m,y),I=k[5][1],J=function(c,a){return b(k[5][4],a,c)},K=f(q[20],J,I,G),L=function(e,c){var
d=c[2],g=c[1],h=k[5][1],i=a(d[2][4],d[1]);function
j(c,a){var
d=b(q[7],g,c)[2];return b(k[5][4],d,a)}var
l=f(am[15],j,i,h);return b(k[5][7],e,l)},M=f(q[20],L,K,H),e=function(c){if(typeof
c==="number")return 0===c?0:1;else
switch(c[0]){case
0:return[0,c[1]];case
1:var
r=c[3],s=c[2],x=c[1];return b(k[5][3],s,M)?[1,x,s,r]:[0,r];case
2:var
y=c[2],l=e(c[1]),m=e(y);if(typeof
l!=="number"&&0===l[0])if(typeof
m!=="number"&&0===m[0]){var
t=h(ao),z=[0,l[1],m[1]],A=i===t?ao[1]:d===t?a(g[2],ao):ao;return[0,a(j[21],[0,A,z])]}return[2,l,m];case
3:var
B=c[2],n=e(c[1]),o=e(B);if(typeof
n!=="number"&&0===n[0])if(typeof
o!=="number"&&0===o[0]){var
u=h(ap),C=[0,n[1],o[1]],D=i===u?ap[1]:d===u?a(g[2],ap):ap;return[0,a(j[21],[0,D,C])]}return[3,n,o];case
4:var
p=e(c[1]);if(typeof
p!=="number"&&0===p[0]){var
v=h(a$),E=[0,p[1]],F=i===v?a$[1]:d===v?a(g[2],a$):a$;return[0,a(j[21],[0,F,E])]}return[4,p];default:var
w=c[2],G=c[3],q=e(c[1]),f=e(G);if(typeof
q!=="number"&&0===q[0]){var
H=q[1];if(w)return f;if(typeof
f!=="number"&&0===f[0])return[0,b(j[33],H,f[1])]}return[5,q,w,f]}},c=e(v),N=hf(u,t,s,r,c)[1],z=function(i,c,h){var
d=c[2],j=c[1];function
k(b,a){return[0,a[1],b]}var
e=b(q[18],k,h);function
m(d){try{var
f=b(q[7],i,d)[1],c=f}catch(b){b=w(b);if(b[1]!==rS)throw b;var
c=a(l[3],rT)}return b(q[38],c,e)}try{var
s=b(d[5],j,m),g=s}catch(c){c=w(c);if(!a(a7[18],c))throw c;var
n=function(a){return a[1]},o=b(q[17],n,e),p=[0,a(d[2],0),o],f=a(d[3],p),r=f?f[1]:a(l[3],rU),g=r}return g},A=b(q[47],m,y),B=function(c){function
e(e){var
g=e[2],j=e[1],i=a(g[2][4],g[1]);function
d(g,f){var
c=g,a=f;for(;;){if(a){var
e=a[2],h=a[1];if(b(am[3],c,i))return[0,h,d(c+1|0,e)];var
c=c+1|0,a=e;continue}return 0}}var
h=d(0,j);return f(k[13],aj,h,c)}var
d=b(q[33],e,A);return z(d[1],d[2],c)},C=b(q[17],B,N),O=he(c);return[0,[0,O,c,fn(p[4],p[5],C)]]}return 0}function
hD(d,c,b){var
e=a(cw[67][4],b);return f(ah[13],d,c,e)}function
bp(aE,aD,aC,aB,aA,k,f){return function(aF,aG){function
c(c){var
m=a(aI[42][4],c),D=a(aI[42][6],c),E=a(aI[42][14],c);try{var
s=[0,a(aI[42][5],c),m],n=hA(s,aE,hr,E,D),t=n[3],u=h(k),N=n[2],O=n[1],o=i===u?k[1]:d===u?a(g[2],k):k,v=h(f),P=i===v?f[1]:d===v?a(g[2],f):f,x=hC(aD,aC,aB,aA,o,aF,t,O,N,s);if(x)var
p=x[1],y=p[2],R=p[3],S=p[1],e=hv(m,P,y),r=e[3],T=e[4],U=e[2],V=e[1],z=function(b){return a(ah[2],b[1])},W=b(q[17],z,r),X=a(F[66][22],W),Y=b(q[17],z,U),Z=a(F[66][22],Y),_=function(a){return[0,b(hE[1],0,[1,[0,a]])]},$=a(a9[1][6],r3),A=hD(a9[1][10][1],$,c),ab=function(b){var
c=b[2];return[0,a(j[10],b[1]),c]},ac=b(q[17],ab,r),B=h(bb),ad=0,ae=[0,o[4]],af=i===B?bb[1]:d===B?a(g[2],bb):bb,ag=[0,Z,[0,X,[0,rN(o,R,a(j[21],[0,af,ae]),ac,T),ad]]],ai=a(F[66][22],ag),aj=fq(m,y),ak=a(q[9],aj),al=function(a){return b(q[7],t,a[2]-1|0)},am=b(q[17],al,r),an=b(l[26],ak,am),ao=b(F[66][3],ai,aG),ap=a(ah[78],0),aq=b(F[66][3],ap,ao),ar=[0,a(j[10],A),an],as=a(j[34],ar),at=[0,a(ah[45],as),0],au=b(q[17],j[10],S),av=[0,a(ah[im],au),at],aw=[0,aq,[0,a(F[66][22],av),0]],ax=_(A),ay=aa(ah[ia],1,r4,ax,V),C=b(F[66][21],ay,aw);else
var
az=a(bn[3],r5),C=b(F[66][4],0,az);return C}catch(c){c=w(c);if(c===L){var
G=a(bn[3],rX);return b(F[66][4],0,G)}if(c===Q[8]){var
H=a(bn[3],rY);return b(F[66][4],0,H)}if(c===fr){a(l[52],l[28]);var
I=b(l[17],r0,rZ),J=b(l[17],r1,I),K=b(l[17],r2,J),M=a(bn[3],K);return b(F[66][4],0,M)}throw c}}return a(cw[67][8],c)}}function
r6(q,p,o){var
b=h(bd),c=i===b?bd[1]:d===b?a(g[2],bd):bd,e=h($),f=i===e?$[1]:d===e?a(g[2],$):$,k=h(as),r=i===k?as[1]:d===k?a(g[2],as):as,l=h(bb),s=[0,r],t=i===l?bb[1]:d===l?a(g[2],bb):bb,m=h(bl),u=a(j[21],[0,t,s]),v=[0,c],w=i===m?bl[1]:d===m?a(g[2],bl):bl,n=a(j[21],[0,w,v]),x=ht(n,function(a){return hl(c,a0,a)},o),y=ef(f,hx(p));function
z(c){var
k=a(aI[42][6],c),l=[0,H(r_,r9,r8),[0,f]],b=h(bk),e=0,m=[0,[0,r$,y,a(j[21],l)],[0,[0,r7,q,u],0]],o=[0,n],p=i===b?bk[1]:d===b?a(g[2],bk):bk,r=hw([0,[0,sa,x,a(j[21],[0,p,o])],m],k),s=[0,a(ah[53],r),e];return a(F[66][22],s)}return a(cw[67][8],z)}function
ej(aF){return function(aG){var
E=p[hY],G=p[119],H=p[121],I=p[122],e=[d,function(m){var
b=h(as);function
f(a){return bR(ag,bD,a)}var
j=i===b?as[1]:d===b?a(g[2],as):as,c=h(bd),k=i===c?bd[1]:d===c?a(g[2],bd):bd,e=h($),l=i===e?$[1]:d===e?a(g[2],$):$;return[0,l,k,bD,j,f]}];function
c(c){var
k=a(aI[42][4],c),J=a(aI[42][6],c),K=a(aI[42][14],c);try{var
r=[0,a(aI[42][5],c),k],m=hA(r,rj,hr,K,J),s=m[3],t=m[2],u=m[1],v=h(e),T=i===v?e[1]:d===v?a(g[2],e):e,U=function(b){var
c=b[2],d=b[1];return[0,d,aT(a(p[73],p[fB]),c)]},V=b(q[17],U,u),x=hC(E,G,H,I,T,aF,s,V,aT(a(p[73],p[fB]),t),r);if(x)var
n=x[1],W=n[3],X=n[2],Y=n[1],Z=function(a){return b(q[31],a[1],Y)},y=hB(b(q[35],Z,u),t),_=y[2],z=bo(X,y[1]),A=h(ea),$=i===A?ea[1]:d===A?a(g[2],ea):ea,f=hv(k,$,z),o=f[3],ab=f[4],ac=f[2],ad=f[1],B=function(b){return a(ah[2],b[1])},ae=b(q[17],B,o),af=a(F[66][22],ae),ag=b(q[17],B,ac),ai=a(F[66][22],ag),aj=function(a){return[0,b(hE[1],0,[1,[0,a]])]},ak=a(a9[1][6],sh),C=hD(a9[1][10][1],ak,c),al=function(b){var
c=b[2];return[0,a(j[10],b[1]),c]},am=[0,ai,[0,af,[0,r6(W,b(q[17],al,o),ab),0]]],an=a(F[66][22],am),ao=fq(k,z),ap=a(q[9],ao),aq=function(a){return b(q[7],s,a[2]-1|0)},ar=b(q[17],aq,o),as=b(l[26],ap,ar),at=b(F[66][3],an,aG),au=a(ah[78],0),av=b(F[66][3],au,at),aw=[0,a(j[10],C),as],ax=a(j[34],aw),ay=[0,a(ah[45],ax),0],az=b(q[17],j[10],_),aA=[0,a(ah[im],az),ay],aB=[0,av,[0,a(F[66][22],aA),0]],aC=aj(C),aD=aa(ah[ia],1,si,aC,ad),D=b(F[66][21],aD,aB);else
var
aE=a(bn[3],sj),D=b(F[66][4],0,aE);return D}catch(c){c=w(c);if(c===L){var
M=a(bn[3],sb);return b(F[66][4],0,M)}if(c===Q[8]){var
N=a(bn[3],sc);return b(F[66][4],0,N)}if(c===fr){a(l[52],l[28]);var
O=b(l[17],se,sd),P=b(l[17],sf,O),R=b(l[17],sg,P),S=a(bn[3],R);return b(F[66][4],0,S)}throw c}}return a(cw[67][8],c)}}var
sk=a(ch[1],[0,aj,bA[27]]),hF=a(sn[8],sm)?0:[d,function(a){throw fr}];function
su(m){var
e=h(hF),p=m[2],r=m[1];if(i!==e)if(d===e)a(g[2],hF);var
n=[0,sr,[0,sq,[0,b(l[17],sp,so[36]),0]]],o=a(ss[3],0),j=f(q[20],st[4],o,n),c=f(k[16],j,[0,j],[0,r,p]);return 0===c[0]?c[1]:a(l[3],c[1])}var
sv=b(sk[6],sl,su);function
hG(c,b){return a(sv,[0,c,b])}function
ek(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,ek(a[2])];default:var
c=a[2],d=a[1],e=ek(a[3]);return[2,ek(d),c,e]}}function
hH(f,c){var
d=hG(f,c);if(d){var
e=a(az[2],d[1]);return b(p[111],c,e)?[0,e]:(a(l[31],sw),0)}return 0}function
cx(e,d,c){function
f(i,h){var
c=i,d=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
g=a(k[8][5],c[1]);return e<=g?b(am[4],g-e|0,d):d;case
2:var
c=c[2];continue;case
3:case
4:var
j=c[1],l=f(c[2],d),c=j,d=l;continue}return d}}return f(c,d)}function
cy(a){return cx(0,am[1],a)}function
a3(b,d){function
c(b){if(typeof
b!=="number")switch(b[0]){case
0:var
e=a(d,a(k[8][5],b[1]));return[0,a(k[7][4],e)];case
2:var
f=b[1];return[2,f,c(b[2])];case
3:var
g=b[1],h=c(b[2]);return[3,c(g),h];case
4:var
i=b[1],j=c(b[2]);return[4,c(i),j]}return b}return c(b)}function
fs(a){function
d(h,g,e){var
b=h,a=g,c=e;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
i=a[2],j=cx(b,c,a[1]),b=b+1|0,a=i,c=j;continue;case
1:var
k=a[2],l=cx(b,c,a[1]),b=b+1|0,a=k,c=l;continue;default:var
m=a[3],n=a[1],o=cx(b,cx(b,c,a[2]),n),p=function(c,a){return d(b+1|0,a,c)};return f(q[20],p,o,m)}}return d(0,a,am[1])}function
ft(c,f){function
d(c,b){return b<c?b:a(f,b-c|0)+c|0}function
e(c,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
f=a[1],g=e(c+1|0,a[2]);return[0,a3(f,function(a){return d(c,a)}),g];case
1:var
h=a[1],i=e(c+1|0,a[2]);return[1,a3(h,function(a){return d(c,a)}),i];default:var
j=a[3],k=a[2],l=a[1],m=function(a){return e(c+1|0,a)},n=b(p[10],m,j),o=a3(k,function(a){return d(c,a)});return[2,a3(l,function(a){return d(c,a)}),o,n]}}return e(0,c)}function
cz(d,c){function
e(b){var
c=b[2];return[0,a(p[71],b[1]),c]}return a(d,b(q[17],e,c))}var
hI=a(ch[1],[0,aj,bA[27]]),sy=a(ch[1],[0,aj,bA[27]]);function
sz(a){var
c=a[1],d=a[2];return cz(b(az[4],c[1],c[2]),d)}var
sB=b(hI[6],sA,sz);function
sC(a){var
c=a[1],d=a[2];return cz(b(az[5],c[1],c[2]),d)}var
sE=b(hI[6],sD,sC);function
sF(b){var
c=b[2];return cz(a(az[6],b[1]),c)}var
sH=b(sy[6],sG,sF);function
sI(b,a){return aG(aH,b,a[1])}function
sJ(a,b){return a1(aH,a,b)}var
sL=[0,sK,fh,function(a){var
c=a[2];return cz(b(az[7],a[1],az[8]),c)},cy,a3,sJ,sI];function
sM(b,a){return aG(aH,b,a[1])}function
sN(a,b){return a1(aH,a,b)}var
sP=[0,sO,fh,function(a){var
c=a[2];return cz(b(az[7],a[1],az[8]),c)},cy,a3,sN,sM];function
sQ(b,a){return aG(aH,b,a[1])}var
hJ=[0,sR,fh,sH,cy,a3,function(a,b){return a1(aH,a,b)},sQ];function
hK(b,a){function
c(b,a){return aG(aH,b,a[1])}function
d(a,b){return a1(aH,a,b)}function
e(a){return hH(a[1],a[2])}return[0,sS,function(c){return[0,b,a]},e,cy,a3,d,c]}function
hL(b,a){function
c(b,a){return aG(aH,b,a[1])}function
d(a,b){return a1(aH,a,b)}function
e(a){return hH(a[1],a[2])}return[0,sT,function(c){return[0,b,a]},e,cy,a3,d,c]}function
hM(d,c){function
e(b,a){return aG(bm,b,a[1])}function
f(f){var
g=f[2],i=f[1];function
h(a){var
b=a[2];return[0,ek(a[1]),b]}var
d=hG(i,b(q[17],h,g));if(d)var
e=a(az[3],d[1]),c=b(p[89],g,e)?[0,e]:(a(l[31],sx),a(l[52],l[28]),0);else
var
c=0;return c?[0,[0,c[1],0]]:0}return[0,sU,function(a){return[0,d,c]},f,fs,ft,bE,e]}var
sW=[0,sV,hc,sB,fs,ft,bE,function(b,a){return aG(bm,b,a[1])}],sY=[0,sX,hc,sE,fs,ft,bE,function(b,a){return aG(bm,b,a[1])}],sZ=a(bp(d8,p[eo],p[er],p[en],p[eq],ei,d$),[0,sL,0]);function
s0(b){var
c=[0,hK(s1,[0,b]),0];return a(bp(d8,p[eo],p[er],p[en],p[eq],ei,d$),c)}var
s2=ej([0,sP,0]);function
s3(a){return ej([0,hL(s4,[0,a]),0])}function
s5(b){var
c=[0,hM(s6,[0,b]),0];return a(bp(d7,p[96],p[94],p[97],p[98],eh,d_),c)}var
s8=[0,hM(s7,0),0],s9=a(bp(d7,p[96],p[94],p[97],p[98],eh,d_),s8),s$=[0,hK(s_,0),0],ta=a(bp(d8,p[eo],p[er],p[en],p[eq],ei,d$),s$),tc=ej([0,hL(tb,0),0]),td=a(bp(d7,p[96],p[94],p[97],p[98],eh,d_),[0,sW,0]),te=a(bp(d7,p[96],p[94],p[97],p[98],eh,d_),[0,sY,0]),tf=ej([0,hJ,0]),U=[0,s5,s0,s3,td,te,tf,a(bp(d8,p[eo],p[er],p[en],p[eq],ei,d$),[0,hJ,0]),s9,ta,tc,sZ,s2,hy];aJ(645,U,"Micromega_plugin.Coq_micromega");a(tg[10],ai);var
th=0,tj=[0,[0,ti,function(a){return ah[55]}],th];V(v[10][8],ai,tk,0,0,tj);var
tl=0;function
tm(d,c){var
e=b(v[13][24],c,d);return a(a(U[1],-1),e)}var
tp=[0,[0,[0,to,[1,[5,a(R[16],v[2][8])],tn,0]],tm],tl];function
tq(d,c,a){var
e=b(v[13][24],a,c);return b(U[1],d,e)}var
ts=[1,[5,a(R[16],v[2][8])],tr,0],tv=[0,[0,[0,tu,[1,[5,a(R[16],fu[6])],tt,ts]],tq],tp];V(v[10][8],ai,tw,0,0,tv);var
tx=0;function
ty(d,c){var
e=b(v[13][24],c,d);return a(U[4],e)}var
tB=[0,[0,[0,tA,[1,[5,a(R[16],v[2][8])],tz,0]],ty],tx];V(v[10][8],ai,tC,0,0,tB);var
tD=0;function
tE(d,c){var
e=b(v[13][24],c,d);return a(U[5],e)}var
tH=[0,[0,[0,tG,[1,[5,a(R[16],v[2][8])],tF,0]],tE],tD];V(v[10][8],ai,tI,0,0,tH);var
tJ=0;function
tK(d,c){var
e=b(v[13][24],c,d);return a(U[6],e)}var
tN=[0,[0,[0,tM,[1,[5,a(R[16],v[2][8])],tL,0]],tK],tJ];V(v[10][8],ai,tO,0,0,tN);var
tP=0;function
tQ(d,c){var
e=b(v[13][24],c,d);return a(U[7],e)}var
tT=[0,[0,[0,tS,[1,[5,a(R[16],v[2][8])],tR,0]],tQ],tP];V(v[10][8],ai,tU,0,0,tT);var
tV=0;function
tW(d,c){var
e=b(v[13][24],c,d);return a(U[8],e)}var
tZ=[0,[0,[0,tY,[1,[5,a(R[16],v[2][8])],tX,0]],tW],tV];V(v[10][8],ai,t0,0,0,tZ);var
t1=0;function
t2(d,c){var
e=b(v[13][24],c,d);return a(U[9],e)}var
t5=[0,[0,[0,t4,[1,[5,a(R[16],v[2][8])],t3,0]],t2],t1];V(v[10][8],ai,t6,0,0,t5);var
t7=0;function
t8(d,c){var
e=b(v[13][24],c,d);return a(U[10],e)}var
t$=[0,[0,[0,t_,[1,[5,a(R[16],v[2][8])],t9,0]],t8],t7];V(v[10][8],ai,ua,0,0,t$);var
ub=0;function
uc(d,c){var
e=b(v[13][24],c,d);return a(U[11],e)}var
uf=[0,[0,[0,ue,[1,[5,a(R[16],v[2][8])],ud,0]],uc],ub];V(v[10][8],ai,ug,0,0,uf);var
uh=0;function
ui(d,c){var
e=b(v[13][24],c,d);return a(U[12],e)}var
ul=[0,[0,[0,uk,[1,[5,a(R[16],v[2][8])],uj,0]],ui],uh];V(v[10][8],ai,um,0,0,ul);var
un=0;function
uo(c,a){var
d=b(v[13][24],a,c);return b(U[3],-1,d)}var
ur=[0,[0,[0,uq,[1,[5,a(R[16],v[2][8])],up,0]],uo],un];function
us(d,c,a){var
e=b(v[13][24],a,c);return b(U[3],d,e)}var
uu=[1,[5,a(R[16],v[2][8])],ut,0],ux=[0,[0,[0,uw,[1,[5,a(R[16],fu[6])],uv,uu]],us],ur];V(v[10][8],ai,uy,0,0,ux);var
uz=0;function
uA(c,a){var
d=b(v[13][24],a,c);return b(U[2],-1,d)}var
uD=[0,[0,[0,uC,[1,[5,a(R[16],v[2][8])],uB,0]],uA],uz];function
uE(d,c,a){var
e=b(v[13][24],a,c);return b(U[2],d,e)}var
uG=[1,[5,a(R[16],v[2][8])],uF,0],uJ=[0,[0,[0,uI,[1,[5,a(R[16],fu[6])],uH,uG]],uE],uD];V(v[10][8],ai,uK,0,0,uJ);var
hN=[0];aJ(650,hN,"Micromega_plugin.G_micromega");aJ(651,[0,fH,p,k,n,Q,az,ch,U,hN],"Micromega_plugin");return}
