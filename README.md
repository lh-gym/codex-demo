# codex-demo

## Testing the R Markdown notebooks

To render `PPO_playground.Rmd` (or any other R Markdown file) during automated testing you need an R runtime that provides the `Rscript` command.

### Installing R inside this container

The container image used here does not ship with R. If your environment allows outbound package downloads, you can install it with:

```bash
sudo apt-get update
sudo apt-get install -y r-base
```

Once R is installed, verify that `Rscript` is on the `PATH`:

```bash
Rscript --version
```

After that you can render the notebook, for example:

```bash
Rscript -e "rmarkdown::render('PPO_playground.Rmd')"
```

> **Note:** In the hosted environment used for these examples, the default proxy blocks `apt-get` traffic (returns HTTP 403). A failed attempt typically ends with messages like `The repository 'http://archive.ubuntu.com/ubuntu noble InRelease' is not signed.` and `Failed to fetch ... 403 Forbidden`. If you encounter the same issue, either switch to a network that allows access to Ubuntu mirrors, configure an internal mirror, or install R locally on your machine and run the render command there instead.

### Alternative: Conda/Mamba

If `apt-get` is unavailable, you can also create a Conda environment with R:

```bash
mamba create -n r-env r-base r-essentials
mamba activate r-env
Rscript --version
```

(Replace `mamba` with `conda` if you do not have Mamba installed.)

After activating the environment, run the same render command shown above.

These steps give you a repeatable way to ensure `Rscript` is available so automated tests can call the notebook render.
