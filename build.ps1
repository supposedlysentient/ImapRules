#! /usr/bin/pwsh

[CmdletBinding()]
param
(
    [Parameter(Mandatory, ParameterSetName = "Release")]
    [ValidateScript({$_})]
    [switch]$Release,

    [Parameter(Mandatory, ParameterSetName = "Docker")]
    [ValidateScript({$_})]
    [switch]$Docker,

    [Parameter(Mandatory, ParameterSetName = "Test")]
    [ValidateScript({$_})]
    [switch]$Test,

    [Parameter(ParameterSetName = "Docker")]
    [AllowEmptyCollection()]
    [ValidatePattern(".")]
    [string[]]$Tags = "latest",

    [Parameter(ParameterSetName = "Docker")]
    [AllowEmptyCollection()]
    [ValidatePattern("\S=")]
    [string[]]$Labels,

    [Parameter(ParameterSetName = "Docker")]
    [ValidateScript({$_.Revision -le 0 -and $_.Build -ge 0})]
    [version]$Version,

    [Parameter(ValueFromRemainingArguments)]
    [string[]]$BuildArgs
)

$Project = "imaprules"
$OciVersionLabel = "org.opencontainers.image.version"

$Target = switch ($PSCmdlet.ParameterSetName) {
    "Release" {"buildRelease"}
    "Docker" {"buildDocker"}
    default {$_}
}

$BuildArgs = switch ($PSCmdlet.ParameterSetName) {
    "Docker"
    {
        if ($Version -and $Labels -match $OciVersionLabel) {
            $Msg = "Cannot provide $OciVersionLabel and Version together."
            throw [System.Management.Automation.ParameterBindingException]::new($Msg)
        }

        if ($Version) {
            $v = "v{0}.{1}.{2}" -f $Version.Major, $Version.Minor, $Version.Build
            $BuildArgs = "--build-arg", "VERSION=$v", $BuildArgs | Write-Output
            $Tags += $v
        }

        if ($Tags) {
            $Tags = $Tags -replace "^(?=[^:]*$)", "$Project`:"
                | % {"--tag", $_}
                | Write-Output
        }

        if ($Labels) {
            [string[]]$Labels = $Labels | % {"--label", $_} | Write-Output
        }

        $Tags, $Labels, $BuildArgs | Write-Output | ? Length
    }

    default {$BuildArgs}
}


dotnet run $Target "--" $BuildArgs
