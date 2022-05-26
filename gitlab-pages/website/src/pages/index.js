import React from 'react';
import Layout from '@theme/Layout';
import useBaseUrl from '@docusaurus/useBaseUrl';
import CodeExamples from '../../core/CodeExamples';

const FEATURES = [
  {
    image: 'img/strong-type-system.svg',
    title: 'Strong, Static Type System',
    content: 'Write types, then code. Benefit from the safety of type systems.'
  },
  {
    image: 'img/syntax-agnostic.svg',
    title: 'Polyglot',
    content:
      'Code in your language. Write PascaLIGO, CameLIGO, ReasonLIGO, JsLIGO or add your own syntax.'
  },
  {
    image: 'img/easy-integration.svg',
    title: 'Easy Integration',
    content: 'You can use LIGO as a Node.js library with Truffle.'
  }
];

const PARTNERS = [
  {
    name: 'Nomadic Labs',
    image: 'img/nomadic-logo.png',
    link: 'https://www.nomadic-labs.com/',
    pinned: true
  },
  {
    name: 'MacStadium developer logo',
    image: 'img/MacStadium-developerlogo.png',
    link: 'https://www.macstadium.com/',
    pinned: true
  },
  {
    name: 'TQ Tezos',
    image: 'img/tq-logo.svg',
    link: 'https://tqtezos.com/',
    pinned: true
  },
  {
    name: 'Stove Labs',
    image: 'img/stove-logo.png',
    link: 'https://stove-labs.com',
    pinned: true
  }
];

const Feature = (props) => (
  <div className="feature" key={props.title}>
    <img src={useBaseUrl(props.image)} />
    <h1>{props.title}</h1>
    <p>{props.content}</p>
  </div>
);

const Partner = (props) => (
  <a
    href={props.link}
    title={props.name}
    target="_blank"
    rel="noopener noreferrer"
  >
    <img src={useBaseUrl(props.image)} />
  </a>
);

function HomePage() {
  return <Layout title="Homepage">
    <div
      id="homePage"
      style={{
        display: 'flex',
        justifyContent: 'stretch',
        alignItems: 'stretch',
        fontSize: '20px',
        flexDirection: 'column'
      }}>
        <div id="intro" className="centered">
          <div id="callToAction">
            <ul>
              <li className="primary">
                <a href="https://ide.ligolang.org">
                  Try Online
                </a>
              </li>
              <li className="primary">
                <a href="https://gitpod.io/#https://gitlab.com/ligolang/template-ligo" target="_blank">
                  Try on Gitpod
                </a>
              </li>
              <li className="secondary">
                <a href={useBaseUrl('/docs/intro/installation')}>
                  Install
                </a>
              </li>
              <li className="secondary">
                <a href={useBaseUrl('https://academy.ligolang.org/')}>
                  Learn on Academy
                </a>
              </li>
          </ul>
        </div>
        <div id="preview">
          <h1>A friendly Smart Contract Language for Tezos</h1>
          <p>Smart contracts were never so easy</p>
          <CodeExamples /> 
        </div>
      </div>
      <div id="features" className="centered">
        {FEATURES.map(entry => 
          <Feature key={entry.title} title={entry.title} content={entry.content} image={entry.image} /> 
         )}
      </div>
      <div id="partners">
          <div className="centered wrapper">
            <span id="heading">Our Partners</span>
            <div id="list">
              {PARTNERS.filter(entry => entry.pinned).map(entry =>
                <Partner key={entry.name} name={entry.name} image={entry.image} link={entry.link} />
              )}
            </div>
          </div>
      </div>
    </div>
  </Layout>
}

export default HomePage;


