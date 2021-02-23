%include polycode.fmt
%include linear.fmt
%include main.fmt
\begin{figure*}
\def\sep{\thickspace\thickspace\thickspace}%
\begin{mathpar}
  \inferrule*{
    %[lab=T-Var]
  }{\pgv{\seq{\pr(\ty{T})}{\bot}{\tmty{x}{T}}{x}{T}}}
  =
  \inferrule*{
    |x :: (ToSesh T)|\vdash|x :: (ToSesh T)|
  }{|return x :: Sesh (Pr (ToSesh T)) Bot (ToSesh T)|}
  \\

  \inferrule*{
    %[lab=T-Lam]
    \pgv{\seq{{p}\sqcap{\pr(\ty{T})}}{q}{\ty{\Gamma},\tmty{x}{T}}{L}{U}}
  }{\pgv{\seq{p}{{\bot}}{\ty{\Gamma}}{\lambda x.L}{\tylolli{p}{q}{T}{U}}}}
  \approx
  \inferrule*{
    |ToSesh Gamma, x :: ToSesh T|\vdash|tosesh L :: Sesh (Min p (Pr (ToSesh T))) q (ToSesh U)|
  }{|return (\x -> tosesh L) :: Sesh (Min p (Pr (ToSesh T))) Bot (ToSesh T %1 -> Sesh (Min p (Pr (ToSesh T))) q (ToSesh U))|}
  \\

  \inferrule*{
    %[lab=T-Const]
  }{\pgv{\seq{{\top}}{{\bot}}{\emptyenv}{K}{T}}}
  =
  \inferrule*{
  }{|return (tosesh K) :: Sesh Top Bot (ToSesh T)|}
  \\

  \inferrule*{
    %[lab=T-App]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tylolli{p''}{q''}{T}{U}}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta}}{M}{T}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
    \sep
    \pgv{\cs{q'}<\cs{p''}}
  }{\pgv{\seq{{p}\sqcap{p'}\sqcap{p''}}{{q}\sqcup{q'}\sqcup{q''}}{\ty{\Gamma},\ty{\Delta}}{L\;M}{U}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T %1 -> Sesh p'' q'' (ToSesh U))|
    \sep
    |ToSesh Delta|\vdash|tosesh M :: Sesh p' q' (ToSesh T)|
  }{|tosesh L >>= \f -> tosesh M >>= \x -> f x :: (q < p', q' < p'') => Sesh (Min (Min p p') p'') (Max (Max q q') q'') (ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-Unit]
  }{\pgv{\seq{{\top}}{{\bot}}{\emptyenv}{\unit}{\tyunit}}}
  =
  \inferrule*{
  }{|return () :: Sesh Top Bot ()|}
  \\

  \inferrule*{
    %[lab=T-LetUnit]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tyunit}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta}}{M}{T}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcap{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\letunit{L}{M}}{T}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q ()|
    \sep
    |ToSesh Delta|\vdash|tosesh M :: Sesh p' q' (ToSesh T)|
  }{|tosesh L >>= \() -> M :: (p < q') => Sesh (Min p p') (Max q q') (ToSesh T)|}
  \\

  \inferrule*{
    %[lab=T-Pair]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{T}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta}}{M}{U}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcap{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\pair{L}{M}}{\typrod{T}{U}}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T)|
    \sep
    |ToSesh Delta|\vdash|tosesh M :: Sesh p' q' (ToSesh U)|
  }{|tosesh L >>= \x -> tosesh M >>= \y -> return (x,y) :: (q < p') => Sesh (Min p p') (Max q q') (ToSesh T, ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-LetPair]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\typrod{T}{T'}}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta},\tmty{x}{T},\tmty{y}{T'}}{M}{U}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcap{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\letpair{x}{y}{L}{M}}{U}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T, ToSesh T')|
    \sep
    |ToSesh Delta, x :: ToSesh T, y :: ToSesh T'|\vdash|tosesh M :: Sesh p' q' (ToSesh U)|
  }{|tosesh L >>= \(x,y) -> M :: (q < p') => Sesh (Min p p') (Max q q') (ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-Inl]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{T}}
    \sep
    \pgv{\cs{\pr(\ty{T})}=\cs{\pr(\ty{U})}}
  }{\pgv{\seq{p}{q}{\ty{\Gamma}}{\inl{L}}{\tysum{T}{U}}}}
  \approx
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh T)|
  }{|ToSesh Gamma|\vdash|tosesh L >>= \x -> return (Left x) :: (q < (Pr (ToSesh T))) => Sesh (Min p (Pr (ToSesh T))) q (Either (ToSesh T) (ToSesh U))|}
  \\

  \inferrule*{
    %[lab=T-Inr]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{T}}
    \sep
    \pgv{\cs{\pr(\ty{T})}=\cs{\pr(\ty{U})}}
  }{\pgv{\seq{p}{q}{\ty{\Gamma}}{\inr{L}}{\tysum{T}{U}}}}
  \approx
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (ToSesh U)|
  }{|ToSesh Gamma|\vdash|tosesh L >>= \x -> return (Right x) :: (q < (Pr (ToSesh U))) => Sesh (Min p (Pr (ToSesh U))) q (Either (ToSesh T) (ToSesh U))|}
  \\

  \inferrule*{
    %[lab=T-CaseSum]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tysum{T}{T'}}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta},\tmty{x}{T}}{M}{U}}
    \sep
    \pgv{\seq{p'}{q'}{\ty{\Delta},\tmty{y}{T'}}{N}{U}}
    \sep
    \pgv{\cs{q}<\cs{p'}}
  }{\pgv{\seq{{p}\sqcup{p'}}{{q}\sqcup{q'}}{\ty{\Gamma},\ty{\Delta}}{\casesum{L}{x}{M}{y}{N}}{U}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q (Either (ToSesh T) (ToSesh T'))|
    \sep
    |ToSesh Delta|\vdash|x :: ToSesh T|\vdash|tosesh M :: Sesh p' q' (ToSesh U)|
    \sep
    |ToSesh Delta|\vdash|y :: ToSesh T'|\vdash|tosesh N :: Sesh p' q' (ToSesh U)|
  }{|ToSesh Gamma, ToSesh Delta|\vdash|tosesh L >>= \x -> case x { Left x -> tosesh M; Right y -> tosesh N } :: Sesh (Min p p') (Max q q') (ToSesh U)|}
  \\

  \inferrule*{
    %[lab=T-Absurd]
    \pgv{\seq{p}{q}{\ty{\Gamma}}{L}{\tyvoid}}
  }{\pgv{\seq{p}{q}{\ty{\Gamma}}{\absurd{L}}{T}}}
  =
  \inferrule*{
    |ToSesh Gamma|\vdash|tosesh L :: Sesh p q Void|
  }{|ToSesh Gamma|\vdash|tosesh L >>= \x -> absurd x :: Sesh p q (ToSesh T)|}
\end{mathpar}
\caption{Translation from Priority GV to Sesh preserves types, approximates priorities.}
\label{fig:pgv-to-sesh-preservation}
\end{figure*}
