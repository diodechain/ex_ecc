defmodule ExEcc.MixProject do
  use Mix.Project

  def project do
    [
      app: :ex_ecc,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Hex
      description: "ExEcc is a library for elliptic curve cryptography.",
      package: [
        licenses: ["Apache-2.0"],
        maintainers: ["Dominic Letz"],
        links: %{"GitHub" => "https://github.com/diodechain/ex_ecc"}
      ],
      # Docs
      name: "ExEcc",
      source_url: "https://github.com/diodechain/ex_ecc",
      docs: [
        # The main page in the docs
        main: "ExEcc",
        extras: ["README.md"]
      ],
      aliases: [
        lint: ["format --check-formatted", "dialyzer"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.3", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.30", only: :dev, runtime: false},
      {:while, "~> 0.2"}
    ]
  end
end
