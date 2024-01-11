const express = require("express");
const process = require("process");

const port = process.env.PORT || 3005;

const app = express();

app.get("/simple", (req, res) => {
  res.send("Simple response!");
});

app.get("/early_hints", async (req, res) => {
  res.writeEarlyHints({ link: ["</some-file.js>", "</some-other-file.js>"] })

  await new Promise((resolve) => { setTimeout(resolve, 1000); });

  res.writeEarlyHints({ link: ["</some-baz.js>"] })

  await new Promise((resolve) => { setTimeout(resolve, 1000); });

  res.send("Early hints response!");
});

app.listen(port, () => {
  console.log(`Example app listening on port ${port}`);
});
