Terraform HCL Converter
=======================

This project will parse simple `.tf` files (no conditionals or complex expressions).

It will output them as the equivalent JSON declarations, to help you get rid of HCL and make equivalent declarations in any domain-specific language of your choice.

- If you know Haskell and are willing to interface with my dodgy parser, you can modify this project to directly output declarations in your chosen format.

- If not, you can use this tool (linux amd64 binary is available in [/releases](https://github.com/tsprlng/terraform-hcl-converter/releases)) to convert to JSON, and then use your favourite language to convert the JSON to your chosen format.

- If you aim to directly target the JSON format yourself, this tool might provide a useful comparison based on original HCL files.


What is this and why?
---------------------

If you've had to deal with even slightly complex or repetitive infrastructure requirements in AWS, GCP, Azure, etc., you have probably used Hashicorp's Terraform to do it.

Terraform is a useful tool that provides a declarative abstraction around these vendors' APIs, and consistent rules for applying changes.

However, its provided way of specifying which infrastructure you would like is pretty dreadful.

- Syntax is awkward, long-winded and inconsistent.

- Built-in formatter insists on aligning equals signs, which leads to useless diffs.

- Ability to inject outside-world state is very limited, and data structures are hard to manipulate.

- Features such as iteration and conditions are poorly-considered afterthoughts with illogical rules and poor type safety.

Luckily, Terraform was originally designed to be used as a flexible backend to other systems, so it can accept declarations in JSON (although the JSON specification is now treated as a second-class citizen).

In defence of the HCL language, they did originally restrict it to pure declarations and recommend using the JSON format once your needs became more complex.

I would go further and say that almost everyone would be better off switching to a more reasonable domain-specific language as soon as possible. Even the first, simplest wrapper I tried, using ERB to make HCL files, gave a more pleasant and consistent experience than using the ambiguous built-in variables and half-baked native loop constructs. Recent additions have not brought any fundamental improvements either -- the inconsistency is getting worse, if anything, while the added flexibility is mostly an illusion compared to any real language.

The maddening weirdness of rules in HCL is why I haven't even tried to support any of its more complex features here. This project only exists because no native converter seems to be provided with Terraform -- it should hopefully be a useful starting point to cut down the bulk of the work in getting away from HCL, but will never cover every case.


Build instructions
------------------

You will need the parsec (parser helpers) and aeson (json de/serialization) libraries. I've used a recent version of GHC. [Stack](https://haskellstack.org) is what I'd recommend to install Haskell as a beginner. However, I've not formatted this project so that you have to use Stack for a build -- do what you like.

Here's a vague example:

```
stack install aeson parsec
stack exec -- ghc -o convert-tf -odir .crap -hidir .crap *.hs
```


Misparsings and missing features
--------------------------------

Please feel free to point out mistakes, suggest improvements or submit patches to support missing HCL features. I can't promise to care about everything, but anyone is always free to copy/fork/mutilate this project to suit any unmet needs.
