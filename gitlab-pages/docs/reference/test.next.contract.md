---
id: test.next.contract-reference
title: contract
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val transfer : &#39;p.&#39;p contract -&gt; &#39;p -&gt; mav -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer: &lt;p&gt;(&#95;: contract&lt;p&gt;) =&gt; (&#95;: p) =&gt; (&#95;: mav) =&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
const transfer : contract (p) -&gt; p -&gt; mav -&gt; test&#95;exec&#95;result
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val transfer&#95;exn : &#39;p.&#39;p contract -&gt; &#39;p -&gt; mav -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let transfer&#95;exn: &lt;p&gt;(&#95;: contract&lt;p&gt;) =&gt; (&#95;: p) =&gt; (&#95;: mav) =&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
const transfer&#95;exn : contract (p) -&gt; p -&gt; mav -&gt; nat
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val to&#95;typed&#95;address : &#39;p &#39;s.&#39;p contract -&gt; (&#39;p, &#39;s) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;typed&#95;address: &lt;p, s&gt;(&#95;: contract&lt;p&gt;) =&gt; typed&#95;address&lt;p, s&gt;
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
const to&#95;typed&#95;address : contract (p) -&gt; typed&#95;address (p, s)
</SyntaxTitle>
