import esbuild from "esbuild";
import mdx from "@mdx-js/esbuild";

await esbuild.build({
  entryPoints: ["input.jsx"],
  bundle: true,  
  plugins: [mdx()],  
});
