import "./styles.scss";
import ligologo from "./logo.svg";

const links = [
  {
    link: "ligolang.org/docs/intro/installation",
    name: "Install",
    isBlank: false,
    isCurrent: false,
  },
  { link: "ligolang.org/docs/intro/introduction", name: "Docs", isBlank: false, isCurrent: false },
  {
    link: "ligolang.org/docs/tutorials/getting-started",
    name: "Tutorials",
    isBlank: false,
    isCurrent: false,
  },
  { link: "packages.ligolang.org/packages", name: "Packages", isBlank: true, isCurrent: false },
  { link: "academy.ligolang.org", name: "Academy", isBlank: true, isCurrent: false },
  { link: "ligo-webide-v2.gcp.marigold.dev", name: "IdeV2 beta", isBlank: true, isCurrent: true },
  { link: "ligolang.org/contact", name: "Ask Questions", isBlank: false, isCurrent: false },
  { link: "ligolang.org/docs/faq/intro", name: "FAQ", isBlank: false, isCurrent: false },
  {
    link: "ligolang.org/docs/next/intro/changelog",
    name: "Changelog",
    isBlank: false,
    isCurrent: false,
  },
];

const LigoHeader = () => {
  return (
    <div className="ligoNavbar">
      <div className="ligoGroup">
        <a className="ligoLink ligoLinkNoPadding" href="////ligolang.org">
          <img className="ligoLogo" src={ligologo} alt="Hello" />
        </a>
        {links.map((l) => {
          return (
            <a
              className={`ligoLink${l.isCurrent ? " ligoLinkActive" : ""}`}
              href={`////${l.link}`}
              target={l.isBlank ? "_blank" : "_self"}
              rel="noreferrer"
            >
              {l.name}
            </a>
          );
        })}
      </div>
      <a
        className="ligoLink ligoLinkCheat"
        href="////ligolang.org/docs/api/cheat-sheet"
        target="_blank"
        rel="noreferrer"
      >
        Cheat Sheet
      </a>
    </div>
  );
};

export default LigoHeader;
