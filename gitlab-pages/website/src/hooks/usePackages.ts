import { useEffect, useState } from "react";

const DEFAULT_PACKAGES = 9;

export default function usePackages() {
  const [packages, setPackages] = useState(DEFAULT_PACKAGES);

  useEffect(() => {
    const abortController = new AbortController();

    fetch("https://packages.ligo.mavryk.org/-/ui/packages", {
      signal: abortController.signal,
    })
      .then((response) => response.json())
      .then((data) => {
        setPackages((data && data.length) || DEFAULT_PACKAGES);
      })
      .catch(() => {
        // Silently fall back to default when fetch fails (e.g., offline/local dev)
      });

    return () => {
      abortController.abort();
    };
  }, []);
  return packages;
}
